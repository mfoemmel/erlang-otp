-module(megaco_text_parser_v3).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("megaco_text_parser_v3.yrl", 1666).

%% The following directive is needed for (significantly) faster compilation
%% of the generated .erl file by the HiPE compiler.  Please do not remove.
-compile([{hipe,[{regalloc,linear_scan}]}]).

-include("megaco_text_parser_v3.hrl").



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



-file("./megaco_text_parser_v3.erl", 164).

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
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_130(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(163=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(164=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_164(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(165=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(166=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(167=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_167(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(168=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(175=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_175(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(176=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_176(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(177=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_177(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(178=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(179=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_179(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(180=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_180(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(181=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(182=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(183=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_183(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(184=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_184(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(185=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(186=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_186(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(187=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_187(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(188=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(189=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_189(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(190=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_190(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(191=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_191(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(192=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_192(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(193=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_193(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(194=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_194(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(195=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_195(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_218(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(219=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(220=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(221=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_221(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(222=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_222(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(223=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_223(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(224=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_224(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(225=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_225(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(226=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_226(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(227=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_227(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(228=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_228(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(229=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_229(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(230=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_230(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(231=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_231(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(232=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_232(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(233=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(234=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_234(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(235=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_235(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(236=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(237=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_237(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(238=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_238(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(239=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_239(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(240=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_240(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(241=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_241(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(242=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_242(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(243=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_243(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(251=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_251(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(252=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(253=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_253(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(254=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(255=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_255(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(256=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_256(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(257=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_257(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(258=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(259=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(260=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_260(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(261=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_261(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(262=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_262(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(263=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_263(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(264=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_264(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(265=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_265(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(266=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_266(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(267=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_267(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(268=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_268(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(269=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_269(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(270=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_270(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(271=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_263(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(272=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_272(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(273=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_273(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(274=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_274(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(275=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(276=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_276(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(277=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(278=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_278(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(279=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_279(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(280=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_280(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(281=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_281(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(282=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_282(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(283=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_283(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(284=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_284(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(285=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_285(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(286=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_286(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(287=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_286(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(288=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_286(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(289=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_289(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(290=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_290(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(296=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_286(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(297=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_286(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(298=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_298(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(299=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_299(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(300=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_286(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(301=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_286(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(302=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_302(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(303=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_303(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(304=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_304(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(305=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_305(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(306=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_306(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(307=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_307(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(308=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_308(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(309=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_309(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(310=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_310(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(311=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_241(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(312=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_312(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(313=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_313(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(314=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_314(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(315=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(316=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_316(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(317=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_317(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(318=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_318(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(319=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_319(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(320=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_320(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(333=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_333(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(334=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_334(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(335=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_335(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(336=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_336(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(337=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_337(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(338=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_338(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(339=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_339(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(340=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_339(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(341=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_341(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(342=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(343=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(344=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(345=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_345(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(346=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_346(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(347=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_347(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(348=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(349=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_349(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(350=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(351=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_351(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(352=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_352(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(353=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_353(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(354=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_354(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(355=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_355(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(356=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_356(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(357=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_357(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(358=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_358(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(359=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_359(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(360=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_360(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(361=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_360(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(362=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_362(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(368=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_368(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(369=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_369(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(370=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_353(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(371=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_371(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(372=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_372(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(373=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_373(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(374=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_374(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(375=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_319(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(376=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_376(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(377=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_377(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(378=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_378(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(379=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(380=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(381=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_381(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(382=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_382(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(383=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_383(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(384=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_384(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(385=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(386=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_386(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(387=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_387(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(388=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(389=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_389(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(390=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_390(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(391=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_391(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(392=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_392(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(393=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_393(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(394=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_394(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(395=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_395(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(396=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_396(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(397=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_397(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(398=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_398(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(399=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_399(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(400=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_400(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(401=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_401(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(402=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_402(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(403=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_403(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(404=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_404(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(405=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_405(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(406=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_406(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(412=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_412(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(413=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_413(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(414=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(415=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_415(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(416=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_416(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(417=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_417(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(418=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_418(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(419=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_419(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(420=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_420(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%%  yeccpars2_243(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(431=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_431(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(432=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_432(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_440(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(441=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_441(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(442=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(443=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_443(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(444=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_444(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(445=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_445(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(446=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_446(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(447=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_286(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(448=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_448(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(449=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(450=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_450(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(451=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_451(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(452=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_452(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(453=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(454=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_454(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(455=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(456=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_456(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(457=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_457(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(458=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_458(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(459=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_419(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(460=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_460(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(461=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_461(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(462=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_462(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(463=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_463(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(464=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(465=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_465(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(466=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(467=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_467(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(468=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_468(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(469=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_469(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(470=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_470(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(471=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_471(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(472=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_472(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(473=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(474=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_474(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(475=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_475(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(476=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_476(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(477=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(478=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_478(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(479=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_479(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(480=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_480(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(481=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_481(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(482=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_482(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(483=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_483(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(484=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_484(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(485=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_485(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(486=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(487=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_487(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(488=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_488(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(489=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_243(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(490=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_490(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(491=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_491(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(492=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(493=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_493(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(494=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_494(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(495=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_495(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(496=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_496(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(497=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_497(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(498=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(499=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_499(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(500=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_500(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(501=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_501(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(502=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_502(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(503=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_503(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(504=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_504(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(505=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_505(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(506=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_506(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(515=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_515(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(528=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_528(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(529=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_529(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(530=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_530(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(531=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_531(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(532=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_532(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(533=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_533(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(534=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_534(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(535=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(536=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_536(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(537=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_537(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(538=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(539=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_539(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(540=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_540(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(541=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_541(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(542=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_542(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(543=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_543(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(544=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_544(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(545=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_545(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(546=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_546(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(547=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_547(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(548=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_548(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(549=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(550=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_550(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(551=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_243(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(552=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_552(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(553=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_553(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(554=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_554(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(555=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_555(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(556=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_556(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(557=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_557(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(558=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(559=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_559(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(560=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_560(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(561=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(562=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_562(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(563=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_563(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(564=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_564(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(565=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_565(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(566=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(567=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_567(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(568=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_568(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(569=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(570=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_570(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(571=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_571(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(572=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_572(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(573=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_573(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_589(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(590=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_590(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(591=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(592=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_592(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(593=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_593(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(594=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_594(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(595=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_595(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(596=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_286(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(597=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_597(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(598=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_598(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(599=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_599(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(600=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_600(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(601=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(602=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_602(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(603=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_603(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(604=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_604(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(605=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_605(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(606=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_606(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(607=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_607(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(608=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_608(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(609=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_609(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(610=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(611=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_611(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(612=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(613=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_613(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(614=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_614(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(615=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(616=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_616(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(617=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_617(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(618=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_618(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(619=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_619(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(620=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_605(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(621=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_621(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(622=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_622(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(623=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_623(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(624=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(625=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_625(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(626=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_626(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(627=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_627(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(628=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(629=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_629(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(630=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_630(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(631=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_631(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(632=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(633=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(634=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_634(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(635=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_635(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(636=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_636(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(637=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(638=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_638(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(639=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_639(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(640=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_640(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(641=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_641(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(642=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(643=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_643(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(644=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_644(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(645=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_645(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(646=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_646(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(647=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_647(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(648=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_648(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(649=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_649(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_339(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(668=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_668(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(669=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_669(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(670=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_670(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(671=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_671(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(672=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_672(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(673=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_673(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(674=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_660(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(675=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_675(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(676=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_676(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(677=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_677(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(678=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(679=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_679(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(680=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_680(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(681=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_681(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(682=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_682(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(683=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_680(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(684=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_684(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(685=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_685(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(686=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_686(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(687=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_687(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(688=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_688(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(689=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_689(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(690=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_690(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(691=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_691(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(692=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_692(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(693=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_502(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(694=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_694(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(695=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_502(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(696=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_696(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(697=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_360(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(698=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_698(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(699=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_699(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(700=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_687(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(701=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_701(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(702=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_702(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(703=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_703(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(704=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_704(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(705=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_648(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(706=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_706(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(707=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_707(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(708=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_708(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(709=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(710=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_710(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(711=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(712=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_712(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(713=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_713(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(714=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_714(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(715=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_715(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(716=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_716(S, Cat, Ss, Stack, T, Ts, Tzr);
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
%% yeccpars2(731=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_731(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(732=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_732(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(733=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_733(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(734=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_734(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(735=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_735(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(736=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_736(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(737=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_737(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(738=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(739=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_739(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(740=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(741=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_741(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(742=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_742(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(743=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_743(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(744=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_744(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(745=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_745(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(746=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_746(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(747=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_747(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(748=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_748(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(749=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_749(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(750=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_750(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(751=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_751(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(752=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_752(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(753=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_753(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(754=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_754(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(755=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_755(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(756=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_756(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(757=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_744(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(758=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_758(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(759=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_759(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(760=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_760(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(761=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_761(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(762=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(763=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_763(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(764=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_764(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(765=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_765(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(766=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_766(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(767=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_767(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(768=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_768(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(769=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_769(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(770=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_770(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(771=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_771(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(772=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_772(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(773=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_773(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(774=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_715(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(775=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_775(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(776=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_776(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(777=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_777(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(778=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_778(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(779=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(780=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_780(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(781=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_781(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(782=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_782(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(783=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_783(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(784=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_784(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(785=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_785(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(786=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_786(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(787=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_787(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(788=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_788(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(789=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_789(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(790=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_790(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(791=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_791(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(792=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_572(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(793=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_793(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(794=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_794(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(795=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_795(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(796=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_796(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(797=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(798=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_798(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(799=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_799(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(800=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_800(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(801=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_801(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(802=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_802(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(803=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(804=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_804(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(805=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_805(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(806=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_806(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(807=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(808=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_808(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(809=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_809(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(810=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_810(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(811=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(812=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_812(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(813=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_813(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(814=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_814(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(815=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(816=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_816(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(956=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(957=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_957(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(958=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_958(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(959=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_959(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(960=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_960(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(961=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_961(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(962=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_962(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(963=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_963(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(964=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_964(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(965=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_965(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
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
yeccpars2_cont_4(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_4(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr).

yeccpars2_5(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_6: see yeccpars2_4

yeccpars2_7(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr).

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

%% yeccpars2_80: see yeccpars2_4

yeccpars2_81(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_82: see yeccpars2_4

yeccpars2_83(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_83_(Stack),
 yeccpars2_84(_S, Cat, [83 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_84_(Stack),
 yeccgoto_authenticationHeader(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_85(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_85_(Stack),
 yeccpars2_88(88, Cat, [85 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_86(S, endOfMessage, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr).

yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_87_(Stack),
 yeccgoto_megacoMessage(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_88(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'MtpAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 963, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_89(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, 'MessageSegmentToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr).

yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_mId(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_mId(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

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
 yeccpars2_95(95, Cat, [93 | Ss], NewStack, T, Ts, Tzr).

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
 yeccpars2_103(_S, Cat, [94 | Ss], NewStack, T, Ts, Tzr).

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
 yeccpars2_97(_S, Cat, [96 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_97_(Stack),
 yeccgoto_daddr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_98(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_98_(Stack),
 yeccgoto_domainAddress(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_99: see yeccpars2_4

yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_100_(Stack),
 yeccgoto_portNumber(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_101(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_101_(Stack),
 yeccpars2_102(_S, Cat, [101 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_102_(Stack),
 yeccgoto_domainAddress(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_103_(Stack),
 yeccgoto_daddr(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_104(S, 'GREATER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr).

yeccpars2_105(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_105_(Stack),
 yeccgoto_domainName(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_106: see yeccpars2_4

yeccpars2_107(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_(Stack),
 yeccpars2_108(_S, Cat, [107 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_108_(Stack),
 yeccgoto_domainName(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_109_(Stack),
 yeccgoto_transactionItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_110_(Stack),
 yeccgoto_transactionItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_111_(Stack),
 yeccgoto_transactionItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_112_(Stack),
 yeccgoto_transactionItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_113_(Stack),
 yeccgoto_messageBody(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_114(S, 'MessageSegmentToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 119, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 121, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 122, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 123, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_114_(Stack),
 yeccgoto_transactionList(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_115_(Stack),
 yeccgoto_transactionItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_116_(Stack),
 yeccgoto_message(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_117_(Stack),
 yeccgoto_messageBody(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_118(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 890, Ss, Stack, T, Ts, Tzr).

yeccpars2_119(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 956, Ss, Stack, T, Ts, Tzr).

yeccpars2_120(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 952, Ss, Stack, T, Ts, Tzr).

yeccpars2_121(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 819, Ss, Stack, T, Ts, Tzr).

yeccpars2_122(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 811, Ss, Stack, T, Ts, Tzr).

yeccpars2_123(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 124, Ss, Stack, T, Ts, Tzr);
yeccpars2_123(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 125, Ss, Stack, T, Ts, Tzr).

yeccpars2_124(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 803, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_125(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 127, Ss, Stack, T, Ts, Tzr).

yeccpars2_126(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 797, Ss, Stack, T, Ts, Tzr);
yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_126_(Stack),
 yeccpars2_796(796, Cat, [126 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_127(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 128, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_128: see yeccpars2_4

yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_129_(Stack),
 yeccgoto_contextID(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_130(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr).

yeccpars2_131(S, 'AddToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 147, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 149, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, 'ContextAttrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 150, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 151, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, 'EmergencyOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 152, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, 'EmergencyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 153, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, 'IEPSToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, 'ModifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, 'MoveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 157, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, 'PriorityToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, 'SubtractToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 160, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, 'TopologyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr).

yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_132_(Stack),
 yeccgoto_contextProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_commandRequest(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_134(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_commandRequest(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_(Stack),
 yeccgoto_contextProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_commandRequest(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_137_(Stack),
 yeccgoto_contextProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_138_(Stack),
 yeccgoto_actionRequestItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_139_(Stack),
 yeccgoto_actionRequestItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_contextProperty(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_141_(Stack),
 yeccgoto_actionRequestItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_commandRequest(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_143(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 569, Ss, Stack, T, Ts, Tzr).

yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_commandRequest(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_145(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 566, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_145_(Stack),
 yeccpars2_565(_S, Cat, [145 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_146(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 564, Ss, Stack, T, Ts, Tzr).

yeccpars2_147(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_147_(Stack),
 yeccgoto_ammToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_148(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 561, Ss, Stack, T, Ts, Tzr).

yeccpars2_149(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 558, Ss, Stack, T, Ts, Tzr).

yeccpars2_150(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 557, Ss, Stack, T, Ts, Tzr).

yeccpars2_151(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 506, Ss, Stack, T, Ts, Tzr).

yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_152_(Stack),
 yeccgoto_contextProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_153(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_153_(Stack),
 yeccgoto_contextProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_154(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 502, Ss, Stack, T, Ts, Tzr).

yeccpars2_155(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_155_(Stack),
 yeccgoto_ammToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_156(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_156_(Stack),
 yeccgoto_ammToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_157(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 466, Ss, Stack, T, Ts, Tzr).

yeccpars2_158(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 464, Ss, Stack, T, Ts, Tzr).

yeccpars2_159(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 414, Ss, Stack, T, Ts, Tzr).

yeccpars2_160(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr).

yeccpars2_161(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr).

yeccpars2_162(S, 'BothwayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 168, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, 'IsolateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, 'OnewayBothToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 170, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, 'OnewayExternalToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 171, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, 'OnewayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 173, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_163(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_topologyDescComp(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_164(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_164(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_164_(Stack),
 yeccpars2_177(177, Cat, [164 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_165(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_165_(Stack),
 yeccgoto_topologyDescComp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_166_(Stack),
 yeccgoto_terminationID(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_167_(Stack),
 yeccgoto_topologyDescComp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_168_(Stack),
 yeccgoto_topologyDirection(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

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

yeccpars2_173(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 174, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_173_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_174: see yeccpars2_4

yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_175_(Stack),
 yeccgoto_eventStream(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_176_(Stack),
 yeccgoto_streamID(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_177(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_178: see yeccpars2_162

yeccpars2_179(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_179_(Stack),
 yeccpars2_180(_S, Cat, [179 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_180_(Stack),
 yeccgoto_topologyDescCompList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_181(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_181_(Stack),
 yeccgoto_topologyDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_182(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_183_(Stack),
 yeccgoto_termIDList(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_184(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_184_(Stack),
 yeccpars2_192(_S, Cat, [184 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_185: see yeccpars2_4

yeccpars2_186(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_186_(Stack),
 yeccpars2_187(187, Cat, [186 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_187(S, 'RSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_188: see yeccpars2_4

yeccpars2_189(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_189_(Stack),
 yeccpars2_190(_S, Cat, [189 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_190(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_190_(Stack),
 yeccgoto_terminationIDListRepeat(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_191(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_191_(Stack),
 yeccgoto_termIDList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_192_(Stack),
 yeccgoto_subtractRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_193(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr).

yeccpars2_194(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 413, Ss, Stack, T, Ts, Tzr).

yeccpars2_195(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr).

yeccpars2_196(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 209, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'DigitMapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 210, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 211, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 212, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 213, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'ModemToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 214, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'MuxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'ObservedEventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 216, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'PackagesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 217, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 218, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 219, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_196_(Stack),
 yeccpars2_208(208, Cat, [196 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_197(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_197_(Stack),
 yeccgoto_auditItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_198_(Stack),
 yeccgoto_indAudauditReturnParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

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

yeccpars2_205(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 405, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_205_(Stack),
 yeccpars2_404(_S, Cat, [205 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_auditItem(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_207(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 401, Ss, Stack, T, Ts, Tzr);
yeccpars2_207(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_207_(Stack),
 yeccpars2_400(_S, Cat, [207 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_208(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 399, Ss, Stack, T, Ts, Tzr).

yeccpars2_209(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_209_(Stack),
 yeccgoto_indAuddigitMapDescriptor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_210(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_210_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_211(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 388, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_211_(Stack),
 yeccgoto_auditItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_212(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 379, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 380, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_212_(Stack),
 yeccgoto_auditItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_213(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 319, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_213_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_214(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_214_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_215(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_215_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_216(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_216_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_217(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 315, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_217_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_218(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 225, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_218_(Stack),
 yeccgoto_auditItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_219(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 220, Ss, Stack, T, Ts, Tzr);
yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_219_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_220: see yeccpars2_4

yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_221_(Stack),
 yeccgoto_pkgdName(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_222(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 223, Ss, Stack, T, Ts, Tzr).

yeccpars2_223(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_223_(Stack),
 yeccgoto_indAudstatisticsDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_224(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_224_(Stack),
 yeccgoto_indAudsignalsDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_225(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 231, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 232, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_226_(Stack),
 yeccgoto_indAudsignalParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_227(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 241, Ss, Stack, T, Ts, Tzr);
yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_227_(Stack),
 yeccgoto_signalRequest(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_signalName(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_229(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 240, Ss, Stack, T, Ts, Tzr).

yeccpars2_230(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_230_(Stack),
 yeccgoto_indAudsignalParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_231(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_231_(Stack),
 yeccgoto_optIndAudsignalParm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_232(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 233, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_232_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_233: see yeccpars2_4

yeccpars2_234(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 236, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_234_(Stack),
 yeccgoto_indAudsignalList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_235(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_235_(Stack),
 yeccgoto_signalListId(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_236: see yeccpars2_4

yeccpars2_237(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_signalListParm(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_238(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 239, Ss, Stack, T, Ts, Tzr).

yeccpars2_239(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_239_(Stack),
 yeccgoto_indAudsignalList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_240(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_240_(Stack),
 yeccgoto_optIndAudsignalParm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_241(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 244, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 245, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'IntsigDelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 246, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 247, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 248, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 249, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 250, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 251, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_241(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_242(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 311, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_242_(Stack),
 yeccpars2_310(310, Cat, [242 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_243(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 285, Ss, Stack, T, Ts, Tzr);
yeccpars2_243(S, 'GREATER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 286, Ss, Stack, T, Ts, Tzr);
yeccpars2_243(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 287, Ss, Stack, T, Ts, Tzr);
yeccpars2_243(S, 'NEQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 288, Ss, Stack, T, Ts, Tzr).

yeccpars2_244(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 279, Ss, Stack, T, Ts, Tzr);
yeccpars2_244(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_244_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_245(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 277, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_245_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_246(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 275, Ss, Stack, T, Ts, Tzr).

yeccpars2_247(_S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_247_COMMA(Stack),
 yeccgoto_sigParameter(hd(Ss), 'COMMA', Ss, NewStack, T, Ts, Tzr);
yeccpars2_247(_S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_247_RBRKT(Stack),
 yeccgoto_sigParameter(hd(Ss), 'RBRKT', Ss, NewStack, T, Ts, Tzr);
yeccpars2_247(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_247_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_248(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 262, Ss, Stack, T, Ts, Tzr);
yeccpars2_248(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_248_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_249(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 259, Ss, Stack, T, Ts, Tzr);
yeccpars2_249(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_249_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_250(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 254, Ss, Stack, T, Ts, Tzr);
yeccpars2_250(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_250_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_251(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 252, Ss, Stack, T, Ts, Tzr);
yeccpars2_251(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_251_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_252: see yeccpars2_4

yeccpars2_253(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_253_(Stack),
 yeccgoto_sigParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_254(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 256, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 257, Ss, Stack, T, Ts, Tzr);
yeccpars2_254(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 258, Ss, Stack, T, Ts, Tzr).

yeccpars2_255(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_255_(Stack),
 yeccgoto_sigParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_256(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_256_(Stack),
 yeccgoto_signalType(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_257(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_257_(Stack),
 yeccgoto_signalType(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_258_(Stack),
 yeccgoto_signalType(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_259: see yeccpars2_4

yeccpars2_260(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_260_(Stack),
 yeccgoto_requestID(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_261(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_261_(Stack),
 yeccgoto_sigParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_262(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 263, Ss, Stack, T, Ts, Tzr).

yeccpars2_263(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 265, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 266, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(S, 'IterationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 267, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 268, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 269, Ss, Stack, T, Ts, Tzr).

yeccpars2_264(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 271, Ss, Stack, T, Ts, Tzr);
yeccpars2_264(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_264_(Stack),
 yeccpars2_270(270, Cat, [264 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_265(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_265_(Stack),
 yeccgoto_notificationReason(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

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

yeccpars2_270(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 274, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_271: see yeccpars2_263

yeccpars2_272(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 271, Ss, Stack, T, Ts, Tzr);
yeccpars2_272(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_272_(Stack),
 yeccpars2_273(_S, Cat, [272 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_273(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_273_(Stack),
 yeccgoto_notificationReasons(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_274(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_274_(Stack),
 yeccgoto_sigParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_275: see yeccpars2_4

yeccpars2_276(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_276_(Stack),
 yeccgoto_sigParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_277: see yeccpars2_4

yeccpars2_278(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_278_(Stack),
 yeccgoto_sigParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_279(S, 'BothToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 281, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'ExternalToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 282, Ss, Stack, T, Ts, Tzr);
yeccpars2_279(S, 'InternalToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 283, Ss, Stack, T, Ts, Tzr).

yeccpars2_280(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_280_(Stack),
 yeccgoto_sigParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_281(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_281_(Stack),
 yeccgoto_direction(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_282(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_282_(Stack),
 yeccgoto_direction(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_283(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_283_(Stack),
 yeccgoto_direction(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_284(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_284_(Stack),
 yeccgoto_sigParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_285(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 296, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 297, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'QuotedChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 291, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_285(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_286(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'QuotedChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 291, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_286(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_287: see yeccpars2_286

%% yeccpars2_288: see yeccpars2_286

yeccpars2_289(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_289_(Stack),
 yeccgoto_parmValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_290(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_290_(Stack),
 yeccgoto_value(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_291(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_291_(Stack),
 yeccgoto_value(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_292(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_292_(Stack),
 yeccgoto_parmValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_293(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_293_(Stack),
 yeccgoto_parmValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_294(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_294_(Stack),
 yeccgoto_alternativeValue(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_295(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_295_(Stack),
 yeccgoto_parmValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_296: see yeccpars2_286

%% yeccpars2_297: see yeccpars2_286

yeccpars2_298(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 300, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 301, Ss, Stack, T, Ts, Tzr);
yeccpars2_298(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_298_(Stack),
 yeccpars2_299(299, Cat, [298 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_299(S, 'RSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 306, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_300: see yeccpars2_286

%% yeccpars2_301: see yeccpars2_286

yeccpars2_302(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 301, Ss, Stack, T, Ts, Tzr);
yeccpars2_302(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_302_(Stack),
 yeccpars2_303(_S, Cat, [302 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_303(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_303_(Stack),
 yeccgoto_valueList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_304(S, 'RSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 305, Ss, Stack, T, Ts, Tzr).

yeccpars2_305(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_305_(Stack),
 yeccgoto_alternativeValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_306(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_306_(Stack),
 yeccgoto_alternativeValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_307(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 301, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_307_(Stack),
 yeccpars2_308(308, Cat, [307 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_308(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 309, Ss, Stack, T, Ts, Tzr).

yeccpars2_309(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_309_(Stack),
 yeccgoto_alternativeValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_310(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 314, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_311: see yeccpars2_241

yeccpars2_312(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 311, Ss, Stack, T, Ts, Tzr);
yeccpars2_312(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_312_(Stack),
 yeccpars2_313(_S, Cat, [312 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_313(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_313_(Stack),
 yeccgoto_sigParameters(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_314(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_314_(Stack),
 yeccgoto_signalRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_315: see yeccpars2_4

yeccpars2_316(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_(Stack),
 yeccgoto_packagesItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_317(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 318, Ss, Stack, T, Ts, Tzr).

yeccpars2_318(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_318_(Stack),
 yeccgoto_indAudpackagesDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_319(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 329, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 330, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 331, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_319(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_cont_319(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 326, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_319(S, 'LocalDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 327, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_319(S, 'RemoteDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 328, Ss, Stack, T, Ts, Tzr).

yeccpars2_320(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_320_(Stack),
 yeccgoto_indAudmediaParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_321(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_321_(Stack),
 yeccgoto_indAudmediaParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_322(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_322_(Stack),
 yeccgoto_indAudmediaParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_323(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_323_(Stack),
 yeccgoto_indAudstreamParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_324(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 375, Ss, Stack, T, Ts, Tzr);
yeccpars2_324(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_324_(Stack),
 yeccpars2_374(374, Cat, [324 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_325(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_325_(Stack),
 yeccgoto_indAudstreamParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_326(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 353, Ss, Stack, T, Ts, Tzr).

yeccpars2_327(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_327_(Stack),
 yeccgoto_indAudstreamParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_328(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_328_(Stack),
 yeccgoto_indAudstreamParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_329(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 220, Ss, Stack, T, Ts, Tzr).

yeccpars2_330(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 348, Ss, Stack, T, Ts, Tzr).

yeccpars2_331(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 332, Ss, Stack, T, Ts, Tzr).

yeccpars2_332(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 337, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 338, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_333(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_333_(Stack),
 yeccgoto_indAudterminationStateParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_334(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 285, Ss, Stack, T, Ts, Tzr);
yeccpars2_334(S, 'GREATER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 286, Ss, Stack, T, Ts, Tzr);
yeccpars2_334(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 287, Ss, Stack, T, Ts, Tzr);
yeccpars2_334(S, 'NEQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 288, Ss, Stack, T, Ts, Tzr);
yeccpars2_334(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_334_(Stack),
 yeccgoto_indAudterminationStateParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_335(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 346, Ss, Stack, T, Ts, Tzr).

yeccpars2_336(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_indAudterminationStateParm(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_337(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_337_(Stack),
 yeccgoto_indAudterminationStateParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_338(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 339, Ss, Stack, T, Ts, Tzr);
yeccpars2_338(S, 'INEQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 340, Ss, Stack, T, Ts, Tzr);
yeccpars2_338(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_338_(Stack),
 yeccgoto_iaServiceStates(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_339(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 342, Ss, Stack, T, Ts, Tzr);
yeccpars2_339(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 343, Ss, Stack, T, Ts, Tzr);
yeccpars2_339(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 344, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_340: see yeccpars2_339

yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_341_(Stack),
 yeccgoto_iaServiceStates(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_342(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_342_(Stack),
 yeccgoto_serviceStatesValue(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_343(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_343_(Stack),
 yeccgoto_serviceStatesValue(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_344(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_344_(Stack),
 yeccgoto_serviceStatesValue(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_345_(Stack),
 yeccgoto_iaServiceStates(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_346(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_346_(Stack),
 yeccgoto_indAudterminationStateDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_347(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_347_(Stack),
 yeccgoto_propertyParm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_348: see yeccpars2_4

yeccpars2_349(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 350, Ss, Stack, T, Ts, Tzr).

yeccpars2_350(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 329, Ss, Stack, T, Ts, Tzr);
yeccpars2_350(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_319(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_351(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 352, Ss, Stack, T, Ts, Tzr).

yeccpars2_352(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_352_(Stack),
 yeccgoto_indAudstreamDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_353(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 357, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 358, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 359, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_353(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_354(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_354_(Stack),
 yeccgoto_indAudlocalParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_355(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 285, Ss, Stack, T, Ts, Tzr);
yeccpars2_355(S, 'GREATER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 286, Ss, Stack, T, Ts, Tzr);
yeccpars2_355(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 287, Ss, Stack, T, Ts, Tzr);
yeccpars2_355(S, 'NEQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 288, Ss, Stack, T, Ts, Tzr);
yeccpars2_355(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_355_(Stack),
 yeccgoto_indAudlocalParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_356(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 370, Ss, Stack, T, Ts, Tzr);
yeccpars2_356(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_356_(Stack),
 yeccpars2_369(369, Cat, [356 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_357(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 360, Ss, Stack, T, Ts, Tzr);
yeccpars2_357(S, 'INEQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 361, Ss, Stack, T, Ts, Tzr);
yeccpars2_357(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_357_(Stack),
 yeccgoto_indAudlocalParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_358(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_358_(Stack),
 yeccgoto_indAudlocalParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_359(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_359_(Stack),
 yeccgoto_indAudlocalParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_360(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 363, Ss, Stack, T, Ts, Tzr);
yeccpars2_360(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 364, Ss, Stack, T, Ts, Tzr);
yeccpars2_360(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 365, Ss, Stack, T, Ts, Tzr);
yeccpars2_360(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 366, Ss, Stack, T, Ts, Tzr);
yeccpars2_360(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 367, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_361: see yeccpars2_360

yeccpars2_362(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_362_(Stack),
 yeccgoto_indAudlocalParm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_363(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_363_(Stack),
 yeccgoto_streamModes(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

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
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_368_(Stack),
 yeccgoto_indAudlocalParm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_369(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 373, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_370: see yeccpars2_353

yeccpars2_371(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 370, Ss, Stack, T, Ts, Tzr);
yeccpars2_371(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_371_(Stack),
 yeccpars2_372(_S, Cat, [371 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_372(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_372_(Stack),
 yeccgoto_indAudlocalParmList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_373(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_373_(Stack),
 yeccgoto_indAudlocalControlDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_374(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 378, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_375: see yeccpars2_319

yeccpars2_376(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 375, Ss, Stack, T, Ts, Tzr);
yeccpars2_376(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_376_(Stack),
 yeccpars2_377(_S, Cat, [376 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_377(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_377_(Stack),
 yeccgoto_indAudmediaParms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_378(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_378_(Stack),
 yeccgoto_indAudmediaDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_379: see yeccpars2_4

%% yeccpars2_380: see yeccpars2_4

yeccpars2_381(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_indAudrequestedEvent(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_382(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 383, Ss, Stack, T, Ts, Tzr).

yeccpars2_383(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_383_(Stack),
 yeccgoto_indAudeventsDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_384(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 385, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_385: see yeccpars2_4

yeccpars2_386(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 387, Ss, Stack, T, Ts, Tzr).

yeccpars2_387(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_387_(Stack),
 yeccgoto_indAudeventsDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_388: see yeccpars2_4

yeccpars2_389(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 393, Ss, Stack, T, Ts, Tzr);
yeccpars2_389(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_389_(Stack),
 yeccpars2_392(_S, Cat, [389 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_390(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 391, Ss, Stack, T, Ts, Tzr).

yeccpars2_391(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_391_(Stack),
 yeccgoto_indAudeventBufferDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_392(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_392_(Stack),
 yeccgoto_indAudeventSpec(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_393(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_393(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_393(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_393(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_393(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_393(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_393(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_393(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_393(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_393(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_393(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_393(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_393(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_393(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_393(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_393(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_393(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 173, Ss, Stack, T, Ts, Tzr);
yeccpars2_393(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_393(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_394(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_394_(Stack),
 yeccgoto_eventParameterName(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_395(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 398, Ss, Stack, T, Ts, Tzr).

yeccpars2_396(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_396_(Stack),
 yeccgoto_indAudeventSpecParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_397(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_397_(Stack),
 yeccgoto_indAudeventSpecParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_398(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_398_(Stack),
 yeccgoto_optIndAudeventSpecParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_399(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_399_(Stack),
 yeccgoto_auditDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_400(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_400_(Stack),
 yeccgoto_auditDescriptorBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_401(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 209, Ss, Stack, T, Ts, Tzr);
yeccpars2_401(S, 'DigitMapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 210, Ss, Stack, T, Ts, Tzr);
yeccpars2_401(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 211, Ss, Stack, T, Ts, Tzr);
yeccpars2_401(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 212, Ss, Stack, T, Ts, Tzr);
yeccpars2_401(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 213, Ss, Stack, T, Ts, Tzr);
yeccpars2_401(S, 'ModemToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 214, Ss, Stack, T, Ts, Tzr);
yeccpars2_401(S, 'MuxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr);
yeccpars2_401(S, 'ObservedEventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 216, Ss, Stack, T, Ts, Tzr);
yeccpars2_401(S, 'PackagesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 217, Ss, Stack, T, Ts, Tzr);
yeccpars2_401(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 218, Ss, Stack, T, Ts, Tzr);
yeccpars2_401(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 219, Ss, Stack, T, Ts, Tzr).

yeccpars2_402(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 401, Ss, Stack, T, Ts, Tzr);
yeccpars2_402(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_402_(Stack),
 yeccpars2_403(_S, Cat, [402 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_403(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_403_(Stack),
 yeccgoto_auditItemList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_404(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_404_(Stack),
 yeccgoto_indAudterminationAudit(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_405(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 209, Ss, Stack, T, Ts, Tzr);
yeccpars2_405(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 407, Ss, Stack, T, Ts, Tzr);
yeccpars2_405(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 408, Ss, Stack, T, Ts, Tzr);
yeccpars2_405(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 409, Ss, Stack, T, Ts, Tzr);
yeccpars2_405(S, 'PackagesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 410, Ss, Stack, T, Ts, Tzr);
yeccpars2_405(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 411, Ss, Stack, T, Ts, Tzr);
yeccpars2_405(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 329, Ss, Stack, T, Ts, Tzr).

yeccpars2_406(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 405, Ss, Stack, T, Ts, Tzr);
yeccpars2_406(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_406_(Stack),
 yeccpars2_412(_S, Cat, [406 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_407(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 388, Ss, Stack, T, Ts, Tzr).

yeccpars2_408(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 379, Ss, Stack, T, Ts, Tzr);
yeccpars2_408(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 380, Ss, Stack, T, Ts, Tzr).

yeccpars2_409(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 319, Ss, Stack, T, Ts, Tzr).

yeccpars2_410(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 315, Ss, Stack, T, Ts, Tzr).

yeccpars2_411(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 225, Ss, Stack, T, Ts, Tzr).

yeccpars2_412(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_412_(Stack),
 yeccgoto_indAudterminationAuditList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_413(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_413_(Stack),
 yeccgoto_optAuditDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_414: see yeccpars2_182

yeccpars2_415(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 416, Ss, Stack, T, Ts, Tzr).

yeccpars2_416(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 418, Ss, Stack, T, Ts, Tzr).

yeccpars2_417(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 463, Ss, Stack, T, Ts, Tzr).

yeccpars2_418(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 419, Ss, Stack, T, Ts, Tzr).

yeccpars2_419(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 433, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 209, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'DigitMapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 210, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 211, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 212, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 213, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 434, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 435, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'ModemToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 214, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'MuxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'ObservedEventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 216, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'PackagesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 217, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 436, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 437, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 438, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'ServiceChangeIncompleteToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 439, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 218, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 219, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 440, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 441, Ss, Stack, T, Ts, Tzr);
yeccpars2_419(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_420(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_420_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_421(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_421_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_422(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_422_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_423(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_423_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_424(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 459, Ss, Stack, T, Ts, Tzr);
yeccpars2_424(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_424_(Stack),
 yeccpars2_458(458, Cat, [424 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_425(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_425_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

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
 yeccgoto_extensionParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_430: see yeccpars2_243

yeccpars2_431(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_431_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_432(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_432_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_433(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 455, Ss, Stack, T, Ts, Tzr);
yeccpars2_433(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_433_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_434(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 453, Ss, Stack, T, Ts, Tzr);
yeccpars2_434(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_434_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_435(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 451, Ss, Stack, T, Ts, Tzr);
yeccpars2_435(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_435_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_436(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 449, Ss, Stack, T, Ts, Tzr);
yeccpars2_436(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_436_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_437(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 447, Ss, Stack, T, Ts, Tzr);
yeccpars2_437(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_437_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_438(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 444, Ss, Stack, T, Ts, Tzr);
yeccpars2_438(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_438_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_439(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_439_(Stack),
 yeccgoto_serviceChangeParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_440(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_440_(Stack),
 yeccgoto_timeStamp(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_441(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 442, Ss, Stack, T, Ts, Tzr);
yeccpars2_441(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_441_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_442: see yeccpars2_4

yeccpars2_443(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_443_(Stack),
 yeccgoto_serviceChangeVersion(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_444(S, 'AuditCapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'AuditValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'AuthToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'BriefToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'ContextAuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'DiscardToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'DisconnectedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'FailoverToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'ForcedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'GracefulToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'H221Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'H223Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'H226Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'HandOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'InSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'InactiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'InterruptByEventToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'InterruptByNewSignalsDescrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'LoopbackToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'Nx64Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'OnOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'OtherReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'OutOfSvcToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'PendingToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'RecvonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'ReplyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'ResponseAckToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'RestartToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'SafeChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'SendonlyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'SendrecvToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'ServicesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'SynchISDNToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'TestToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'TimeOutToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'TransToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'V18Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 70, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'V22Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'V22bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'V32Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'V32bisToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'V34Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'V76Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'V90Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'V91Token', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_444(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_444_(Stack),
 yeccpars2_88(88, Cat, [444 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_445(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_445_(Stack),
 yeccgoto_serviceChangeAddress(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_446(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_446_(Stack),
 yeccgoto_serviceChangeAddress(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_447: see yeccpars2_286

yeccpars2_448(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_448_(Stack),
 yeccgoto_serviceChangeReason(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_449: see yeccpars2_4

yeccpars2_450(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_450_(Stack),
 yeccgoto_serviceChangeProfile(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_451(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_451(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_451(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_451(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_451_(Stack),
 yeccpars2_88(88, Cat, [451 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_452(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_452_(Stack),
 yeccgoto_serviceChangeMgcId(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_453: see yeccpars2_4

yeccpars2_454(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_454_(Stack),
 yeccgoto_serviceChangeMethod(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_455: see yeccpars2_4

yeccpars2_456(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_456_(Stack),
 yeccgoto_serviceChangeDelay(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_457(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_457_(Stack),
 yeccgoto_extension(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_458(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 462, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_459: see yeccpars2_419

yeccpars2_460(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 459, Ss, Stack, T, Ts, Tzr);
yeccpars2_460(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_460_(Stack),
 yeccpars2_461(_S, Cat, [460 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_461(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_461_(Stack),
 yeccgoto_serviceChangeParms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_462(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_462_(Stack),
 yeccgoto_serviceChangeDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_463(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_463_(Stack),
 yeccgoto_serviceChangeRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_464: see yeccpars2_4

yeccpars2_465(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_465_(Stack),
 yeccgoto_priority(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_466: see yeccpars2_182

yeccpars2_467(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 468, Ss, Stack, T, Ts, Tzr).

yeccpars2_468(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_468(S, 'ObservedEventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 472, Ss, Stack, T, Ts, Tzr).

yeccpars2_469(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_469_(Stack),
 yeccgoto_notifyRequestBody(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_470(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 501, Ss, Stack, T, Ts, Tzr).

yeccpars2_471(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_471_(Stack),
 yeccgoto_notifyRequestBody(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_472(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 473, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_473: see yeccpars2_4

yeccpars2_474(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 475, Ss, Stack, T, Ts, Tzr).

yeccpars2_475(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 440, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_475_(Stack),
 yeccpars2_4(477, Cat, [475 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_476(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_476(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_476_(Stack),
 yeccpars2_496(496, Cat, [476 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_477: see yeccpars2_4

yeccpars2_478(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 480, Ss, Stack, T, Ts, Tzr);
yeccpars2_478(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_478_(Stack),
 yeccpars2_479(479, Cat, [478 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_479(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 483, Ss, Stack, T, Ts, Tzr).

yeccpars2_480(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_480(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 440, Ss, Stack, T, Ts, Tzr);
yeccpars2_480(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_480_(Stack),
 yeccpars2_4(477, Cat, [480 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_481(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 480, Ss, Stack, T, Ts, Tzr);
yeccpars2_481(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_481_(Stack),
 yeccpars2_482(_S, Cat, [481 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_482(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_482_(Stack),
 yeccgoto_observedEvents(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_483(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_483_(Stack),
 yeccgoto_observedEventsDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_484(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 486, Ss, Stack, T, Ts, Tzr);
yeccpars2_484(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_484_(Stack),
 yeccpars2_485(_S, Cat, [484 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_485(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_485_(Stack),
 yeccgoto_observedEvent(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_486: see yeccpars2_4

yeccpars2_487(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 492, Ss, Stack, T, Ts, Tzr);
yeccpars2_487(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_487_(Stack),
 yeccpars2_491(491, Cat, [487 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_488(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_observedEventParameter(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_489: see yeccpars2_243

yeccpars2_490(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_490_(Stack),
 yeccgoto_eventStreamOrOther(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_491(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 495, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_492: see yeccpars2_4

yeccpars2_493(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 492, Ss, Stack, T, Ts, Tzr);
yeccpars2_493(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_493_(Stack),
 yeccpars2_494(_S, Cat, [493 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_494(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_494_(Stack),
 yeccgoto_observedEventParameters(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_495(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_495_(Stack),
 yeccgoto_observedEventBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_496(S, 'COLON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 497, Ss, Stack, T, Ts, Tzr).

yeccpars2_497(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_497(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_497_(Stack),
 yeccpars2_4(498, Cat, [497 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_498: see yeccpars2_4

yeccpars2_499(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 486, Ss, Stack, T, Ts, Tzr);
yeccpars2_499(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_499_(Stack),
 yeccpars2_500(_S, Cat, [499 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_500(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_500_(Stack),
 yeccgoto_observedEvent(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_501(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_501_(Stack),
 yeccgoto_notifyRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_502(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 504, Ss, Stack, T, Ts, Tzr);
yeccpars2_502(S, 'OnToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 505, Ss, Stack, T, Ts, Tzr).

yeccpars2_503(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_503_(Stack),
 yeccgoto_iepsValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_504(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_504_(Stack),
 yeccgoto_onOrOff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_505(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_505_(Stack),
 yeccgoto_onOrOff(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_506(S, 'AndAUDITselectToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 516, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(S, 'ContextAttrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 517, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(S, 'EmergencyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 518, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(S, 'EmergencyValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 519, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(S, 'IEPSToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 520, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(S, 'OrAUDITselectToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 521, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(S, 'PriorityToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 522, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(S, 'TopologyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 523, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_506(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_507(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_507_(Stack),
 yeccgoto_contextAuditSelector(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_508(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_508_(Stack),
 yeccgoto_contextAuditProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_509(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 556, Ss, Stack, T, Ts, Tzr).

yeccpars2_510(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_510_(Stack),
 yeccgoto_contextAuditSelector(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_511(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_511_(Stack),
 yeccgoto_contextAuditSelector(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_512(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_contextAuditProperty(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_513(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 543, Ss, Stack, T, Ts, Tzr);
yeccpars2_513(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_513_(Stack),
 yeccpars2_554(554, Cat, [513 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_514(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_contextAuditSelector(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_515(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_515_(Stack),
 yeccgoto_contextAuditSelector(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_516(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_516_(Stack),
 yeccgoto_auditSelectLogic(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_517(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 527, Ss, Stack, T, Ts, Tzr).

yeccpars2_518(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_518_(Stack),
 yeccgoto_contextAuditProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_519(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 524, Ss, Stack, T, Ts, Tzr).

yeccpars2_520(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 502, Ss, Stack, T, Ts, Tzr);
yeccpars2_520(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_520_(Stack),
 yeccgoto_contextAuditProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_521(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_521_(Stack),
 yeccgoto_auditSelectLogic(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_522(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 464, Ss, Stack, T, Ts, Tzr);
yeccpars2_522(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_522_(Stack),
 yeccgoto_contextAuditProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_523(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_523_(Stack),
 yeccgoto_contextAuditProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_524(S, 'EmergencyOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 525, Ss, Stack, T, Ts, Tzr);
yeccpars2_524(S, 'EmergencyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 526, Ss, Stack, T, Ts, Tzr).

yeccpars2_525(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_525_(Stack),
 yeccgoto_emergencyValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_526(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_526_(Stack),
 yeccgoto_emergencyValue(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_527(S, 'AndAUDITselectToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 516, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, 'ContextAttrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 150, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, 'ContextListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 533, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, 'EmergencyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 518, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, 'EmergencyValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 519, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, 'IEPSToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 520, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, 'OrAUDITselectToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 521, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, 'PriorityToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 522, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, 'TopologyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 523, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_527(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_528(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 553, Ss, Stack, T, Ts, Tzr).

yeccpars2_529(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 549, Ss, Stack, T, Ts, Tzr);
yeccpars2_529(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_529_(Stack),
 yeccpars2_548(_S, Cat, [529 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_530(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 285, Ss, Stack, T, Ts, Tzr);
yeccpars2_530(S, 'GREATER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 286, Ss, Stack, T, Ts, Tzr);
yeccpars2_530(S, 'LESSER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 287, Ss, Stack, T, Ts, Tzr);
yeccpars2_530(S, 'NEQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 288, Ss, Stack, T, Ts, Tzr);
yeccpars2_530(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_530_(Stack),
 yeccgoto_contextAuditProperty(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_531(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 547, Ss, Stack, T, Ts, Tzr).

yeccpars2_532(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 543, Ss, Stack, T, Ts, Tzr);
yeccpars2_532(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_532_(Stack),
 yeccpars2_542(542, Cat, [532 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_533(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 534, Ss, Stack, T, Ts, Tzr).

yeccpars2_534(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 535, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_535: see yeccpars2_4

yeccpars2_536(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 538, Ss, Stack, T, Ts, Tzr);
yeccpars2_536(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_536_(Stack),
 yeccpars2_537(537, Cat, [536 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_537(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 541, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_538: see yeccpars2_4

yeccpars2_539(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 538, Ss, Stack, T, Ts, Tzr);
yeccpars2_539(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_539_(Stack),
 yeccpars2_540(_S, Cat, [539 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_540(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_540_(Stack),
 yeccgoto_contextIDs(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_541(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_541_(Stack),
 yeccgoto_contextIdList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_542(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 546, Ss, Stack, T, Ts, Tzr).

yeccpars2_543(S, 'AndAUDITselectToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 516, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(S, 'ContextAttrToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 150, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(S, 'EmergencyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 518, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(S, 'EmergencyValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 519, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(S, 'IEPSToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 520, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(S, 'OrAUDITselectToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 521, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(S, 'PriorityToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 522, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(S, 'TopologyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 523, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_544(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 543, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_544_(Stack),
 yeccpars2_545(_S, Cat, [544 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_545(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_545_(Stack),
 yeccgoto_contextAuditProperties(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_546(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_546_(Stack),
 yeccgoto_indAudcontextAttrDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_547(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_547_(Stack),
 yeccgoto_contextAttrDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_548(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_548_(Stack),
 yeccgoto_propertyParms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_549: see yeccpars2_4

yeccpars2_550(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 549, Ss, Stack, T, Ts, Tzr);
yeccpars2_550(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_550_(Stack),
 yeccpars2_552(_S, Cat, [550 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_551: see yeccpars2_243

yeccpars2_552(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_552_(Stack),
 yeccgoto_propertyParmList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_553(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_553_(Stack),
 yeccgoto_contextAttrDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_554(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 555, Ss, Stack, T, Ts, Tzr).

yeccpars2_555(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_555_(Stack),
 yeccgoto_contextAudit(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_556(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_556_(Stack),
 yeccgoto_contextAudit(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_557(S, 'ContextListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 533, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_558: see yeccpars2_182

yeccpars2_559(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_559(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_559_(Stack),
 yeccpars2_560(_S, Cat, [559 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_560(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_560_(Stack),
 yeccgoto_auditRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_561: see yeccpars2_182

yeccpars2_562(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_562(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_562_(Stack),
 yeccpars2_563(_S, Cat, [562 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_563(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_563_(Stack),
 yeccgoto_auditRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_564(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_564_(Stack),
 yeccgoto_actionRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_565(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_565_(Stack),
 yeccgoto_actionRequestBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_566: see yeccpars2_131

yeccpars2_567(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 566, Ss, Stack, T, Ts, Tzr);
yeccpars2_567(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_567_(Stack),
 yeccpars2_568(_S, Cat, [567 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_568(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_568_(Stack),
 yeccgoto_actionRequestItems(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_569: see yeccpars2_182

yeccpars2_570(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 572, Ss, Stack, T, Ts, Tzr);
yeccpars2_570(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_570_(Stack),
 yeccpars2_571(_S, Cat, [570 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_571(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_571_(Stack),
 yeccgoto_ammRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_572(S, 'AuditToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_572(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 583, Ss, Stack, T, Ts, Tzr);
yeccpars2_572(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 584, Ss, Stack, T, Ts, Tzr);
yeccpars2_572(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 585, Ss, Stack, T, Ts, Tzr);
yeccpars2_572(S, 'MediaToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 586, Ss, Stack, T, Ts, Tzr);
yeccpars2_572(S, 'ModemToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 587, Ss, Stack, T, Ts, Tzr);
yeccpars2_572(S, 'MuxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 588, Ss, Stack, T, Ts, Tzr);
yeccpars2_572(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 589, Ss, Stack, T, Ts, Tzr);
yeccpars2_572(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 590, Ss, Stack, T, Ts, Tzr).

yeccpars2_573(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_573_(Stack),
 yeccgoto_ammParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

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

yeccpars2_582(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 792, Ss, Stack, T, Ts, Tzr);
yeccpars2_582(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_582_(Stack),
 yeccpars2_791(791, Cat, [582 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_583(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_583_(Stack),
 yeccgoto_digitMapDescriptor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_584(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 783, Ss, Stack, T, Ts, Tzr);
yeccpars2_584(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_584_(Stack),
 yeccgoto_eventBufferDescriptor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_585(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 709, Ss, Stack, T, Ts, Tzr);
yeccpars2_585(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_585_(Stack),
 yeccgoto_eventsDescriptor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_586(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 648, Ss, Stack, T, Ts, Tzr).

yeccpars2_587(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 632, Ss, Stack, T, Ts, Tzr);
yeccpars2_587(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 633, Ss, Stack, T, Ts, Tzr).

yeccpars2_588(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 624, Ss, Stack, T, Ts, Tzr).

yeccpars2_589(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 605, Ss, Stack, T, Ts, Tzr);
yeccpars2_589(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_589_(Stack),
 yeccgoto_signalsDescriptor(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_590(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 591, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_591: see yeccpars2_4

yeccpars2_592(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 601, Ss, Stack, T, Ts, Tzr);
yeccpars2_592(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_592_(Stack),
 yeccpars2_600(600, Cat, [592 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_593(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 594, Ss, Stack, T, Ts, Tzr);
yeccpars2_593(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_593_(Stack),
 yeccgoto_statisticsParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_594(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 596, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'QuotedChars', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 291, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_595(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_595_(Stack),
 yeccgoto_statisticsParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_596: see yeccpars2_286

yeccpars2_597(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 301, Ss, Stack, T, Ts, Tzr);
yeccpars2_597(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_597_(Stack),
 yeccpars2_598(598, Cat, [597 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_598(S, 'RSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 599, Ss, Stack, T, Ts, Tzr).

yeccpars2_599(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_599_(Stack),
 yeccgoto_statisticsParameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_600(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 604, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_601: see yeccpars2_4

yeccpars2_602(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 601, Ss, Stack, T, Ts, Tzr);
yeccpars2_602(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_602_(Stack),
 yeccpars2_603(_S, Cat, [602 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_603(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_603_(Stack),
 yeccgoto_statisticsParameters(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_604(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_604_(Stack),
 yeccgoto_statisticsDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_605(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_605(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_605(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_605(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_605(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_605(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_605(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_605(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_605(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_605(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_605(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_605(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_605(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_605(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_605(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 609, Ss, Stack, T, Ts, Tzr);
yeccpars2_605(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_605(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_605(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_605(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_606(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_606_(Stack),
 yeccgoto_signalParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_607(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 620, Ss, Stack, T, Ts, Tzr);
yeccpars2_607(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_607_(Stack),
 yeccpars2_619(619, Cat, [607 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_608(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_608_(Stack),
 yeccgoto_signalParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_609(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 610, Ss, Stack, T, Ts, Tzr);
yeccpars2_609(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_609_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_610: see yeccpars2_4

yeccpars2_611(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 612, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_612: see yeccpars2_4

yeccpars2_613(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 615, Ss, Stack, T, Ts, Tzr);
yeccpars2_613(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_613_(Stack),
 yeccpars2_614(614, Cat, [613 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_614(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 618, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_615: see yeccpars2_4

yeccpars2_616(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 615, Ss, Stack, T, Ts, Tzr);
yeccpars2_616(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_616_(Stack),
 yeccpars2_617(_S, Cat, [616 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_617(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_617_(Stack),
 yeccgoto_signalListParms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_618(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_618_(Stack),
 yeccgoto_signalList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_619(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 623, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_620: see yeccpars2_605

yeccpars2_621(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 620, Ss, Stack, T, Ts, Tzr);
yeccpars2_621(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_621_(Stack),
 yeccpars2_622(_S, Cat, [621 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_622(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_622_(Stack),
 yeccgoto_signalParms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_623(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_623_(Stack),
 yeccgoto_signalsDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_624: see yeccpars2_4

yeccpars2_625(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_625_(Stack),
 yeccgoto_muxType(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_626(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 628, Ss, Stack, T, Ts, Tzr).

yeccpars2_627(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_627_(Stack),
 yeccgoto_muxDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_628: see yeccpars2_4

yeccpars2_629(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 188, Ss, Stack, T, Ts, Tzr);
yeccpars2_629(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_629_(Stack),
 yeccpars2_630(630, Cat, [629 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_630(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 631, Ss, Stack, T, Ts, Tzr).

yeccpars2_631(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_631_(Stack),
 yeccgoto_terminationIDList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_632: see yeccpars2_4

%% yeccpars2_633: see yeccpars2_4

yeccpars2_634(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_634_(Stack),
 yeccgoto_modemType(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_635(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 637, Ss, Stack, T, Ts, Tzr);
yeccpars2_635(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_635_(Stack),
 yeccpars2_636(636, Cat, [635 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_636(S, 'RSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 640, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_637: see yeccpars2_4

yeccpars2_638(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 637, Ss, Stack, T, Ts, Tzr);
yeccpars2_638(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_638_(Stack),
 yeccpars2_639(_S, Cat, [638 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_639(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_639_(Stack),
 yeccgoto_modemTypeList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_640(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 642, Ss, Stack, T, Ts, Tzr);
yeccpars2_640(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_640_(Stack),
 yeccpars2_641(_S, Cat, [640 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_641(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_641_(Stack),
 yeccgoto_modemDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_642: see yeccpars2_4

yeccpars2_643(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 549, Ss, Stack, T, Ts, Tzr);
yeccpars2_643(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_643_(Stack),
 yeccpars2_644(644, Cat, [643 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_644(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 645, Ss, Stack, T, Ts, Tzr).

yeccpars2_645(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_645_(Stack),
 yeccgoto_optPropertyParms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_646(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 642, Ss, Stack, T, Ts, Tzr);
yeccpars2_646(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_646_(Stack),
 yeccpars2_647(_S, Cat, [646 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_647(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_647_(Stack),
 yeccgoto_modemDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_648(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 590, Ss, Stack, T, Ts, Tzr);
yeccpars2_648(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 658, Ss, Stack, T, Ts, Tzr);
yeccpars2_648(S, 'TerminationStateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 659, Ss, Stack, T, Ts, Tzr);
yeccpars2_648(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_648(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_cont_648(S, 'LocalControlToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 655, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_648(S, 'LocalDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 656, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_648(S, 'RemoteDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 657, Ss, Stack, T, Ts, Tzr).

yeccpars2_649(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_649_(Stack),
 yeccgoto_mediaParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_650(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_650_(Stack),
 yeccgoto_mediaParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_651(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_651_(Stack),
 yeccgoto_mediaParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_652(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_652_(Stack),
 yeccgoto_streamParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_653(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 705, Ss, Stack, T, Ts, Tzr);
yeccpars2_653(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_653_(Stack),
 yeccpars2_704(704, Cat, [653 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_654(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_654_(Stack),
 yeccgoto_streamParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_655(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 687, Ss, Stack, T, Ts, Tzr).

yeccpars2_656(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_656_(Stack),
 yeccgoto_streamParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_657(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_657_(Stack),
 yeccgoto_streamParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_658(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 678, Ss, Stack, T, Ts, Tzr).

yeccpars2_659(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 660, Ss, Stack, T, Ts, Tzr).

yeccpars2_660(S, 'BufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 665, Ss, Stack, T, Ts, Tzr);
yeccpars2_660(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_660(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_660(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_660(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_660(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_660(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_660(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_660(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_660(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_660(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_660(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_660(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_660(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_660(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_660(S, 'ServiceStatesToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 666, Ss, Stack, T, Ts, Tzr);
yeccpars2_660(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_660(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_660(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_660(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_660(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_661(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 674, Ss, Stack, T, Ts, Tzr);
yeccpars2_661(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_661_(Stack),
 yeccpars2_673(673, Cat, [661 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_662(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_662_(Stack),
 yeccgoto_terminationStateParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_663(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_663_(Stack),
 yeccgoto_terminationStateParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_664(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_664_(Stack),
 yeccgoto_terminationStateParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_665(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 669, Ss, Stack, T, Ts, Tzr).

yeccpars2_666(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 667, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_667: see yeccpars2_339

yeccpars2_668(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_668_(Stack),
 yeccgoto_serviceStates(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_669(S, 'LockStepToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 671, Ss, Stack, T, Ts, Tzr);
yeccpars2_669(S, 'OffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 672, Ss, Stack, T, Ts, Tzr).

yeccpars2_670(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_670_(Stack),
 yeccgoto_eventBufferControl(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_671(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_671_(Stack),
 yeccgoto_eventBufferControlValue(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_672(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_672_(Stack),
 yeccgoto_eventBufferControlValue(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_673(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 677, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_674: see yeccpars2_660

yeccpars2_675(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 674, Ss, Stack, T, Ts, Tzr);
yeccpars2_675(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_675_(Stack),
 yeccpars2_676(_S, Cat, [675 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_676(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_676_(Stack),
 yeccgoto_terminationStateParms(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_677(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_677_(Stack),
 yeccgoto_terminationStateDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_678: see yeccpars2_4

yeccpars2_679(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 680, Ss, Stack, T, Ts, Tzr).

yeccpars2_680(S, 'StatsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 590, Ss, Stack, T, Ts, Tzr);
yeccpars2_680(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_648(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_681(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 683, Ss, Stack, T, Ts, Tzr);
yeccpars2_681(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_681_(Stack),
 yeccpars2_682(682, Cat, [681 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_682(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 686, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_683: see yeccpars2_680

yeccpars2_684(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 683, Ss, Stack, T, Ts, Tzr);
yeccpars2_684(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_684_(Stack),
 yeccpars2_685(_S, Cat, [684 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_685(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_685_(Stack),
 yeccgoto_streamParmList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_686(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_686_(Stack),
 yeccgoto_streamDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_687(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_687(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_687(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_687(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_687(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_687(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_687(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_687(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_687(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_687(S, 'ModeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 690, Ss, Stack, T, Ts, Tzr);
yeccpars2_687(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_687(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_687(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_687(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_687(S, 'ReservedGroupToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 691, Ss, Stack, T, Ts, Tzr);
yeccpars2_687(S, 'ReservedValueToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 692, Ss, Stack, T, Ts, Tzr);
yeccpars2_687(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_687(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_687(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_687(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_687(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_687(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_688(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_688_(Stack),
 yeccgoto_localParm(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_689(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 700, Ss, Stack, T, Ts, Tzr);
yeccpars2_689(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_689_(Stack),
 yeccpars2_699(699, Cat, [689 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_690(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 697, Ss, Stack, T, Ts, Tzr).

yeccpars2_691(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 695, Ss, Stack, T, Ts, Tzr).

yeccpars2_692(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 693, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_693: see yeccpars2_502

yeccpars2_694(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_694_(Stack),
 yeccgoto_localParm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_695: see yeccpars2_502

yeccpars2_696(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_696_(Stack),
 yeccgoto_localParm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_697: see yeccpars2_360

yeccpars2_698(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_698_(Stack),
 yeccgoto_localParm(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_699(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 703, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_700: see yeccpars2_687

yeccpars2_701(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 700, Ss, Stack, T, Ts, Tzr);
yeccpars2_701(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_701_(Stack),
 yeccpars2_702(_S, Cat, [701 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_702(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_702_(Stack),
 yeccgoto_localParmList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_703(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_703_(Stack),
 yeccgoto_localControlDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_704(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 708, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_705: see yeccpars2_648

yeccpars2_706(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 705, Ss, Stack, T, Ts, Tzr);
yeccpars2_706(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_706_(Stack),
 yeccpars2_707(_S, Cat, [706 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_707(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_707_(Stack),
 yeccgoto_mediaParmList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_708(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_708_(Stack),
 yeccgoto_mediaDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_709: see yeccpars2_4

yeccpars2_710(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 711, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_711: see yeccpars2_4

yeccpars2_712(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 779, Ss, Stack, T, Ts, Tzr);
yeccpars2_712(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_712_(Stack),
 yeccpars2_778(778, Cat, [712 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_713(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 715, Ss, Stack, T, Ts, Tzr);
yeccpars2_713(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_713_(Stack),
 yeccpars2_714(_S, Cat, [713 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_714(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_714_(Stack),
 yeccgoto_requestedEvent(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_715(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_715(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_715(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 723, Ss, Stack, T, Ts, Tzr);
yeccpars2_715(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_715(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_715(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 724, Ss, Stack, T, Ts, Tzr);
yeccpars2_715(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_715(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 725, Ss, Stack, T, Ts, Tzr);
yeccpars2_715(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_715(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_715(S, 'NeverNotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 726, Ss, Stack, T, Ts, Tzr);
yeccpars2_715(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_715(S, 'NotifyImmediateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 727, Ss, Stack, T, Ts, Tzr);
yeccpars2_715(S, 'NotifyRegulatedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 728, Ss, Stack, T, Ts, Tzr);
yeccpars2_715(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_715(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_715(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_715(S, 'ResetEventsDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 729, Ss, Stack, T, Ts, Tzr);
yeccpars2_715(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_715(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_715(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_715(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_715(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_715(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_716(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_716_(Stack),
 yeccgoto_notifyBehaviour(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_717(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_717_(Stack),
 yeccgoto_eventParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_718(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_eventParameter(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_719(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 774, Ss, Stack, T, Ts, Tzr);
yeccpars2_719(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_719_(Stack),
 yeccpars2_773(773, Cat, [719 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_720(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_eventParameter(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_721(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_eventParameter(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_722(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_eventParameter(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_723(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_723_(Stack),
 yeccgoto_eventDM(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_724(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 734, Ss, Stack, T, Ts, Tzr);
yeccpars2_724(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_724_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_725(_S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_725_COMMA(Stack),
 yeccgoto_eventParameter(hd(Ss), 'COMMA', Ss, NewStack, T, Ts, Tzr);
yeccpars2_725(_S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_725_RBRKT(Stack),
 yeccgoto_eventParameter(hd(Ss), 'RBRKT', Ss, NewStack, T, Ts, Tzr);
yeccpars2_725(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_725_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_726(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_726_(Stack),
 yeccgoto_notifyBehaviour(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_727(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_727_(Stack),
 yeccgoto_notifyBehaviour(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_728(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 730, Ss, Stack, T, Ts, Tzr);
yeccpars2_728(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_728_(Stack),
 yeccgoto_notifyRegulated(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_729(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_729_(Stack),
 yeccgoto_eventParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_730(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 733, Ss, Stack, T, Ts, Tzr).

yeccpars2_731(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 772, Ss, Stack, T, Ts, Tzr).

yeccpars2_732(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 771, Ss, Stack, T, Ts, Tzr).

yeccpars2_733(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 734, Ss, Stack, T, Ts, Tzr).

yeccpars2_734(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 737, Ss, Stack, T, Ts, Tzr);
yeccpars2_734(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 589, Ss, Stack, T, Ts, Tzr).

yeccpars2_735(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 767, Ss, Stack, T, Ts, Tzr);
yeccpars2_735(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 768, Ss, Stack, T, Ts, Tzr).

yeccpars2_736(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 766, Ss, Stack, T, Ts, Tzr).

yeccpars2_737(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 738, Ss, Stack, T, Ts, Tzr);
yeccpars2_737(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_737_(Stack),
 yeccgoto_embedFirst(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_738: see yeccpars2_4

yeccpars2_739(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 740, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_740: see yeccpars2_4

yeccpars2_741(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 762, Ss, Stack, T, Ts, Tzr);
yeccpars2_741(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_741_(Stack),
 yeccpars2_761(761, Cat, [741 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_742(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 744, Ss, Stack, T, Ts, Tzr);
yeccpars2_742(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_742_(Stack),
 yeccpars2_743(_S, Cat, [742 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_743(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_743_(Stack),
 yeccgoto_secondRequestedEvent(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_744(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_744(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_744(S, 'DigitMapDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 723, Ss, Stack, T, Ts, Tzr);
yeccpars2_744(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_744(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_744(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 750, Ss, Stack, T, Ts, Tzr);
yeccpars2_744(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_744(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 751, Ss, Stack, T, Ts, Tzr);
yeccpars2_744(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_744(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_744(S, 'NeverNotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 726, Ss, Stack, T, Ts, Tzr);
yeccpars2_744(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_744(S, 'NotifyImmediateToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 727, Ss, Stack, T, Ts, Tzr);
yeccpars2_744(S, 'NotifyRegulatedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 728, Ss, Stack, T, Ts, Tzr);
yeccpars2_744(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_744(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_744(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_744(S, 'ResetEventsDescriptorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 752, Ss, Stack, T, Ts, Tzr);
yeccpars2_744(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_744(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_744(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_744(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_744(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_744(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_745(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 757, Ss, Stack, T, Ts, Tzr);
yeccpars2_745(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_745_(Stack),
 yeccpars2_756(756, Cat, [745 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_746(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_746_(Stack),
 yeccgoto_secondEventParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_747(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondEventParameter(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_748(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondEventParameter(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_749(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_secondEventParameter(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_750(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 753, Ss, Stack, T, Ts, Tzr);
yeccpars2_750(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_750_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_751(_S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_751_COMMA(Stack),
 yeccgoto_secondEventParameter(hd(Ss), 'COMMA', Ss, NewStack, T, Ts, Tzr);
yeccpars2_751(_S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_751_RBRKT(Stack),
 yeccgoto_secondEventParameter(hd(Ss), 'RBRKT', Ss, NewStack, T, Ts, Tzr);
yeccpars2_751(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_751_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_752(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_752_(Stack),
 yeccgoto_secondEventParameter(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_753(S, 'SignalsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 589, Ss, Stack, T, Ts, Tzr).

yeccpars2_754(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 755, Ss, Stack, T, Ts, Tzr).

yeccpars2_755(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_755_(Stack),
 yeccgoto_embedSig(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_756(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 760, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_757: see yeccpars2_744

yeccpars2_758(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 757, Ss, Stack, T, Ts, Tzr);
yeccpars2_758(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_758_(Stack),
 yeccpars2_759(_S, Cat, [758 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_759(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_759_(Stack),
 yeccgoto_secondEventParameters(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_760(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_760_(Stack),
 yeccgoto_secondRequestedEventBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_761(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 765, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_762: see yeccpars2_4

yeccpars2_763(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 762, Ss, Stack, T, Ts, Tzr);
yeccpars2_763(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_763_(Stack),
 yeccpars2_764(_S, Cat, [763 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_764(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_764_(Stack),
 yeccgoto_secondRequestedEvents(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_765(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_765_(Stack),
 yeccgoto_embedFirst(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_766(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_766_(Stack),
 yeccgoto_embedNoSig(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_767(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 737, Ss, Stack, T, Ts, Tzr).

yeccpars2_768(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_768_(Stack),
 yeccgoto_embedWithSig(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_769(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 770, Ss, Stack, T, Ts, Tzr).

yeccpars2_770(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_770_(Stack),
 yeccgoto_embedWithSig(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_771(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_771_(Stack),
 yeccgoto_notifyRegulated(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_772(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_772_(Stack),
 yeccgoto_notifyRegulated(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_773(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 777, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_774: see yeccpars2_715

yeccpars2_775(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 774, Ss, Stack, T, Ts, Tzr);
yeccpars2_775(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_775_(Stack),
 yeccpars2_776(_S, Cat, [775 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_776(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_776_(Stack),
 yeccgoto_eventParameters(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_777(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_777_(Stack),
 yeccgoto_requestedEventBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_778(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 782, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_779: see yeccpars2_4

yeccpars2_780(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 779, Ss, Stack, T, Ts, Tzr);
yeccpars2_780(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_780_(Stack),
 yeccpars2_781(_S, Cat, [780 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_781(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_781_(Stack),
 yeccgoto_requestedEvents(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_782(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_782_(Stack),
 yeccgoto_eventsDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_783(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_783(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 440, Ss, Stack, T, Ts, Tzr);
yeccpars2_783(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_783_(Stack),
 yeccpars2_4(477, Cat, [783 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_784(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_784_(Stack),
 yeccgoto_eventSpec(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_785(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 787, Ss, Stack, T, Ts, Tzr);
yeccpars2_785(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_785_(Stack),
 yeccpars2_786(786, Cat, [785 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_786(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 790, Ss, Stack, T, Ts, Tzr).

yeccpars2_787(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_787(S, 'TimeStampToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 440, Ss, Stack, T, Ts, Tzr);
yeccpars2_787(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_787_(Stack),
 yeccpars2_4(477, Cat, [787 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_788(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 787, Ss, Stack, T, Ts, Tzr);
yeccpars2_788(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_788_(Stack),
 yeccpars2_789(_S, Cat, [788 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_789(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_789_(Stack),
 yeccgoto_eventSpecList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_790(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_790_(Stack),
 yeccgoto_eventBufferDescriptor(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_791(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 795, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_792: see yeccpars2_572

yeccpars2_793(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 792, Ss, Stack, T, Ts, Tzr);
yeccpars2_793(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_793_(Stack),
 yeccpars2_794(_S, Cat, [793 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_794(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_794_(Stack),
 yeccgoto_ammParameters(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_795(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_795_(Stack),
 yeccgoto_ammRequestBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_796(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 800, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_797: see yeccpars2_125

yeccpars2_798(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 797, Ss, Stack, T, Ts, Tzr);
yeccpars2_798(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_798_(Stack),
 yeccpars2_799(_S, Cat, [798 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_799(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_799_(Stack),
 yeccgoto_actionRequestList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_800(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_800_(Stack),
 yeccgoto_transactionRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_801(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 807, Ss, Stack, T, Ts, Tzr).

yeccpars2_802(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_802_(Stack),
 yeccgoto_transactionID(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_803: see yeccpars2_125

yeccpars2_804(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 797, Ss, Stack, T, Ts, Tzr);
yeccpars2_804(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_804_(Stack),
 yeccpars2_805(805, Cat, [804 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_805(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 806, Ss, Stack, T, Ts, Tzr).

yeccpars2_806(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_806_(Stack),
 yeccgoto_transactionRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_807: see yeccpars2_125

yeccpars2_808(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 797, Ss, Stack, T, Ts, Tzr);
yeccpars2_808(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_808_(Stack),
 yeccpars2_809(809, Cat, [808 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_809(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 810, Ss, Stack, T, Ts, Tzr).

yeccpars2_810(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_810_(Stack),
 yeccgoto_transactionRequest(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_811: see yeccpars2_4

yeccpars2_812(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 815, Ss, Stack, T, Ts, Tzr);
yeccpars2_812(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_812_(Stack),
 yeccpars2_814(814, Cat, [812 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_813(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_813_(Stack),
 yeccgoto_transactionAck(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_814(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 818, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_815: see yeccpars2_4

yeccpars2_816(S, 'COMMA', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 815, Ss, Stack, T, Ts, Tzr);
yeccpars2_816(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_816_(Stack),
 yeccpars2_817(_S, Cat, [816 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_817(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_817_(Stack),
 yeccgoto_transactionAckList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_818(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_818_(Stack),
 yeccgoto_transactionResponseAck(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_819: see yeccpars2_4

yeccpars2_820(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 822, Ss, Stack, T, Ts, Tzr).

yeccpars2_821(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_821_(Stack),
 yeccgoto_transactionID_and_segment_info(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_822(S, 'ImmAckRequiredToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 824, Ss, Stack, T, Ts, Tzr);
yeccpars2_822(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_822_(Stack),
 yeccpars2_823(823, Cat, [822 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_823(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 829, Ss, Stack, T, Ts, Tzr);
yeccpars2_823(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr).

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
 yeccpars1(S, 150, Ss, Stack, T, Ts, Tzr);
yeccpars2_832(S, 'EmergencyOffToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 152, Ss, Stack, T, Ts, Tzr);
yeccpars2_832(S, 'EmergencyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 153, Ss, Stack, T, Ts, Tzr);
yeccpars2_832(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_832(S, 'IEPSToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_832(S, 'ModifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 845, Ss, Stack, T, Ts, Tzr);
yeccpars2_832(S, 'MoveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 846, Ss, Stack, T, Ts, Tzr);
yeccpars2_832(S, 'NotifyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 847, Ss, Stack, T, Ts, Tzr);
yeccpars2_832(S, 'PriorityToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_832(S, 'ServiceChangeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 848, Ss, Stack, T, Ts, Tzr);
yeccpars2_832(S, 'SubtractToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 849, Ss, Stack, T, Ts, Tzr);
yeccpars2_832(S, 'TopologyToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr).

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

%% yeccpars2_850: see yeccpars2_182

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
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
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
 yeccpars1(S, 440, Ss, Stack, T, Ts, Tzr);
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
 yeccpars1(S, 451, Ss, Stack, T, Ts, Tzr).

yeccpars2_865(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 449, Ss, Stack, T, Ts, Tzr).

yeccpars2_866(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 444, Ss, Stack, T, Ts, Tzr).

yeccpars2_867(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 442, Ss, Stack, T, Ts, Tzr).

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

%% yeccpars2_875: see yeccpars2_182

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
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr).

yeccpars2_879(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 880, Ss, Stack, T, Ts, Tzr).

yeccpars2_880(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_880_(Stack),
 yeccgoto_notifyReplyBody(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_881(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 884, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_881(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
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
 NewStack = yeccpars2_884_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_885(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_885_(Stack),
 yeccgoto_contextTerminationAudit(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_886(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_886_(Stack),
 yeccgoto_auditReply(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_887(S, 'CtxToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'DelayToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 889, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_887(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_888(S, 'RBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 897, Ss, Stack, T, Ts, Tzr).

yeccpars2_889(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 890, Ss, Stack, T, Ts, Tzr);
yeccpars2_889(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_889_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

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
 yeccpars1(S, 583, Ss, Stack, T, Ts, Tzr);
yeccpars2_898(S, 'DigitMapToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 210, Ss, Stack, T, Ts, Tzr);
yeccpars2_898(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 118, Ss, Stack, T, Ts, Tzr);
yeccpars2_898(S, 'EventBufferToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 584, Ss, Stack, T, Ts, Tzr);
yeccpars2_898(S, 'EventsToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 585, Ss, Stack, T, Ts, Tzr);
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
 yeccpars1(S, 589, Ss, Stack, T, Ts, Tzr);
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
 yeccpars1(S, 648, Ss, Stack, T, Ts, Tzr);
yeccpars2_913(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_913_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_914(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 632, Ss, Stack, T, Ts, Tzr);
yeccpars2_914(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 633, Ss, Stack, T, Ts, Tzr);
yeccpars2_914(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_914_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_915(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 624, Ss, Stack, T, Ts, Tzr);
yeccpars2_915(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_915_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_916(S, 'EQUAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 473, Ss, Stack, T, Ts, Tzr);
yeccpars2_916(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_916_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_917(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 919, Ss, Stack, T, Ts, Tzr);
yeccpars2_917(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_917_(Stack),
 yeccgoto_auditReturnItem(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_918(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 591, Ss, Stack, T, Ts, Tzr);
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
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'DirectionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'DurationToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'EmbedToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'ErrorToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'KeepActiveToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'LSBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 185, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'MethodToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'MgcIdToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'NotifyCompletionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'ProfileToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'ReasonToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'RequestIDToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'ServiceChangeAddressToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'SignalListToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'SignalTypeToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'StreamToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, 'VersionToken', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_931(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_4(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_932(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_932_(Stack),
 yeccgoto_auditReply(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_933(S, 'LBRKT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 887, Ss, Stack, T, Ts, Tzr);
yeccpars2_933(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_933_(Stack),
 yeccgoto_safeToken(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_934(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_934_(Stack),
 yeccgoto_auditReply(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_935(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_935_(Stack),
 yeccgoto_actionReply(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_936: see yeccpars2_182

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

%% yeccpars2_956: see yeccpars2_4

yeccpars2_957(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_957_(Stack),
 yeccgoto_segmentReply(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_958(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_958_(Stack),
 yeccgoto_transactionList(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_959(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_959_(Stack),
 yeccgoto_pathName(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_960(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_960_(Stack),
 yeccgoto_deviceName(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_961(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_961(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_961_(Stack),
 yeccpars2_965(_S, Cat, [961 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_962(S, 'SEP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_962(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_962_(Stack),
 yeccpars2_964(_S, Cat, [962 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_963(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_963_(Stack),
 yeccgoto_mtpAddress(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_964(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_964_(Stack),
 yeccgoto_mId(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_965(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_965_(Stack),
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

yeccgoto_actionRequest(125, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(126, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionRequest(797, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_798(798, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionRequest(803, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_804(804, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionRequest(807, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_808(808, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_actionRequestBody(131, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(146, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_actionRequestItem(131, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(145, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionRequestItem(566, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_567(567, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_actionRequestItems(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_565(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionRequestItems(567=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_568(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_actionRequestList(126, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_796(796, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionRequestList(798=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_799(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionRequestList(804, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_805(805, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_actionRequestList(808, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_809(809, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_alternativeValue(285=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_295(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ammParameter(572, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_582(582, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ammParameter(792, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_793(793, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ammParameters(582, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_791(791, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ammParameters(793=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_794(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ammRequest(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ammRequest(566=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ammRequestBody(570=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_571(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_ammToken(131, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(143, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_ammToken(566, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(143, Cat, Ss, Stack, T, Ts, Tzr).

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

yeccgoto_auditDescriptor(193, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_194(194, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditDescriptor(572=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_581(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditDescriptor(792=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_581(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditDescriptorBody(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(208, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditItem(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_207(207, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditItem(401, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_402(402, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditItem(419=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_432(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditItem(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_432(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditItemList(207=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_400(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditItemList(402=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_403(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditOther(881=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_883(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditOther(931=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_932(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditReply(832=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_838(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditReply(943=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_838(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditRequest(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditRequest(566=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_auditReturnItem(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditReturnItem(401=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditReturnItem(419=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditReturnItem(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr);
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

yeccgoto_auditSelectLogic(506=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_515(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditSelectLogic(527=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_515(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_auditSelectLogic(543=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_515(_S, Cat, Ss, Stack, T, Ts, Tzr).

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

yeccgoto_commandRequest(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_commandRequest(566=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextAttrDescriptor(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAttrDescriptor(506=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_514(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAttrDescriptor(527=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_514(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAttrDescriptor(543=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_514(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAttrDescriptor(566=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAttrDescriptor(832=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAttrDescriptor(943=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextAudit(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAudit(566=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextAuditProperties(513, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_554(554, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAuditProperties(532, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_542(542, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAuditProperties(544=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_545(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextAuditProperty(506, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_513(513, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAuditProperty(527, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_532(532, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAuditProperty(543, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_544(544, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextAuditSelector(506=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_512(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAuditSelector(527=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_512(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextAuditSelector(543=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_512(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextID(128, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(130, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextID(535, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_536(536, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextID(538, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_539(539, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextID(830, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_831(831, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextIDs(536, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_537(537, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextIDs(539=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_540(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextIdList(527, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_531(531, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextIdList(557, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_531(531, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextProperty(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextProperty(566=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextProperty(832=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_836(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextProperty(943=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_836(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_contextTerminationAudit(884=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_886(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_contextTerminationAudit(933=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_934(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_daddr(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(95, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_daddr(94=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_daddr(96=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_deviceName(88, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_962(962, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_digitMapDescriptor(572=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_580(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_digitMapDescriptor(792=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_580(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_digitMapDescriptor(898=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_910(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_digitMapDescriptor(927=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_910(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_digitMapDescriptor(939=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_910(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_direction(279=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_280(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_domainAddress(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_domainAddress(444=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_domainAddress(451=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_domainName(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_domainName(444=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_domainName(451=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_embedFirst(734, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_736(736, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_embedFirst(767, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_769(769, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_embedNoSig(715=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_722(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_embedNoSig(730, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_732(732, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_embedNoSig(774=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_722(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_embedSig(744=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_749(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_embedSig(757=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_749(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_embedWithSig(715=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_721(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_embedWithSig(730, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_731(731, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_embedWithSig(774=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_721(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_emergencyValue(506=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_511(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_emergencyValue(527=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_511(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_emergencyValue(543=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_511(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_errorCode(890, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_892(892, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_errorDescriptor(89=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_errorDescriptor(468=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_471(_S, Cat, Ss, Stack, T, Ts, Tzr);
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

yeccgoto_eventBufferControl(660=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_664(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventBufferControl(674=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_664(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventBufferControlValue(669=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_670(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventBufferDescriptor(572=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_579(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventBufferDescriptor(792=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_579(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventBufferDescriptor(898=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_908(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventBufferDescriptor(927=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_908(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventBufferDescriptor(939=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_908(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventDM(715=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_720(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventDM(744=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_748(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventDM(757=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_748(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventDM(774=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_720(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventParameter(715, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_719(719, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventParameter(774, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_775(775, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventParameterName(393=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_397(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventParameterName(486, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(489, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventParameterName(492, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(489, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventParameterName(715, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(489, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventParameterName(744, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(489, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventParameterName(757, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(489, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventParameterName(774, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(489, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventParameters(719, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_773(773, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventParameters(775=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_776(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventSpec(783, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_785(785, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventSpec(787, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_788(788, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventSpecList(785, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_786(786, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventSpecList(788=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_789(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventStream(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventStream(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventStream(393=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_396(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventStreamOrOther(486=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_488(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventStreamOrOther(492=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_488(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventStreamOrOther(715=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_718(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventStreamOrOther(744=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_747(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventStreamOrOther(757=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_747(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventStreamOrOther(774=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_718(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_eventsDescriptor(572=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_578(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventsDescriptor(792=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_578(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventsDescriptor(898=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_907(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventsDescriptor(927=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_907(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_eventsDescriptor(939=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_907(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_extension(419=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_431(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_extension(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_431(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_extensionParameter(419, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(430, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_extensionParameter(459, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(430, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_iaServiceStates(332=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_336(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_iepsValue(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_iepsValue(506=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_510(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_iepsValue(527=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_510(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_iepsValue(543=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_510(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_iepsValue(566=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_iepsValue(832=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_iepsValue(943=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudauditReturnParameter(196, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(205, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudauditReturnParameter(401, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(205, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudauditReturnParameter(405, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_406(406, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudauditReturnParameter(419, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(205, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudauditReturnParameter(459, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(205, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudcontextAttrDescriptor(506, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_509(509, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAuddigitMapDescriptor(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAuddigitMapDescriptor(401=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAuddigitMapDescriptor(405=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAuddigitMapDescriptor(419=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAuddigitMapDescriptor(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudeventBufferDescriptor(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudeventBufferDescriptor(401=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudeventBufferDescriptor(405=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudeventBufferDescriptor(419=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudeventBufferDescriptor(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudeventSpec(388, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_390(390, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudeventSpecParameter(393, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_395(395, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudeventsDescriptor(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudeventsDescriptor(401=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudeventsDescriptor(405=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudeventsDescriptor(419=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudeventsDescriptor(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudlocalControlDescriptor(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_325(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudlocalControlDescriptor(350=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_325(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudlocalControlDescriptor(375=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_325(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudlocalParm(353, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_356(356, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudlocalParm(370, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_371(371, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudlocalParmList(356, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_369(369, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudlocalParmList(371=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_372(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudmediaDescriptor(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudmediaDescriptor(401=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudmediaDescriptor(405=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudmediaDescriptor(419=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudmediaDescriptor(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudmediaParm(319, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_324(324, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudmediaParm(375, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_376(376, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudmediaParms(324, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_374(374, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudmediaParms(376=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_377(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudpackagesDescriptor(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudpackagesDescriptor(401=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudpackagesDescriptor(405=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudpackagesDescriptor(419=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudpackagesDescriptor(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudrequestedEvent(380, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_382(382, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudrequestedEvent(385, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_386(386, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudsignalList(225=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudsignalParm(225, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(229, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudsignalsDescriptor(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudsignalsDescriptor(401=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudsignalsDescriptor(405=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudsignalsDescriptor(419=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudsignalsDescriptor(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudstatisticsDescriptor(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstatisticsDescriptor(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_323(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstatisticsDescriptor(350=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_323(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstatisticsDescriptor(375=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_323(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstatisticsDescriptor(401=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstatisticsDescriptor(405=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstatisticsDescriptor(419=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstatisticsDescriptor(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudstreamDescriptor(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_322(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstreamDescriptor(375=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_322(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudstreamParm(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_321(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstreamParm(350, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_351(351, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudstreamParm(375=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_321(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudterminationAudit(196=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_197(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudterminationAudit(401=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_197(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudterminationAudit(419=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_197(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudterminationAudit(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_197(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudterminationAuditList(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_404(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudterminationAuditList(406=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_412(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudterminationStateDescriptor(319=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_320(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_indAudterminationStateDescriptor(375=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_320(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_indAudterminationStateParm(332, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_335(335, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_localControlDescriptor(648=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_654(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_localControlDescriptor(680=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_654(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_localControlDescriptor(683=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_654(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_localControlDescriptor(705=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_654(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_localParm(687, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_689(689, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_localParm(700, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_701(701, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_localParmList(689, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_699(699, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_localParmList(701=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_702(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mId(85, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(89, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mId(444=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_446(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mId(451=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_452(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mediaDescriptor(572=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_577(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mediaDescriptor(792=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_577(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mediaDescriptor(898=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_906(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mediaDescriptor(927=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_906(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mediaDescriptor(939=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_906(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mediaParm(648, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_653(653, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mediaParm(705, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_706(706, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mediaParmList(653, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_704(704, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_mediaParmList(706=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_707(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_megacoMessage(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_message(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(86, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_messageBody(89=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_modemDescriptor(572=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_576(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_modemDescriptor(792=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_576(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_modemDescriptor(898=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_905(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_modemDescriptor(927=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_905(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_modemDescriptor(939=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_905(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_modemType(632, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_646(646, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_modemType(633, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_635(635, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_modemType(637, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_638(638, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_modemTypeList(635, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_636(636, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_modemTypeList(638=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_639(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_mtpAddress(88, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_961(961, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_muxDescriptor(572=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_575(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_muxDescriptor(792=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_575(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_muxDescriptor(898=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_904(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_muxDescriptor(927=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_904(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_muxDescriptor(939=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_904(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_muxType(624, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_626(626, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_notificationReason(263, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_264(264, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_notificationReason(271, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_272(272, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_notificationReasons(264, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_270(270, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_notificationReasons(272=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_273(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_notifyBehaviour(715=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_717(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_notifyBehaviour(744=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_746(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_notifyBehaviour(757=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_746(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_notifyBehaviour(774=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_717(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_notifyRegulated(715=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_716(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_notifyRegulated(744=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_716(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_notifyRegulated(757=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_716(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_notifyRegulated(774=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_716(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_notifyReply(832=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_834(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_notifyReply(943=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_834(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_notifyReplyBody(876=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_877(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_notifyRequest(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_notifyRequest(566=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_notifyRequestBody(468, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_470(470, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_observedEvent(475, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_478(478, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEvent(480, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_481(481, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEvent(783=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_784(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEvent(787=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_784(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_observedEventBody(484=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_485(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEventBody(499=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_500(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_observedEventParameter(486, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_487(487, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEventParameter(492, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_493(493, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_observedEventParameters(487, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_491(491, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEventParameters(493=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_494(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_observedEvents(478, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_479(479, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEvents(481=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_482(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_observedEventsDescriptor(468=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_469(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEventsDescriptor(898=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_903(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEventsDescriptor(927=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_903(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_observedEventsDescriptor(939=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_903(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_onOrOff(502=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_503(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_onOrOff(693=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_694(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_onOrOff(695=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_696(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_optAuditDescriptor(184=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optAuditDescriptor(559=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_560(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optAuditDescriptor(562=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_563(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_optImmAckRequired(822, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_823(823, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_optIndAudeventSpecParameter(389=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_392(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_optIndAudsignalParm(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_224(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optIndAudsignalParm(411=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_224(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_optPropertyParms(640=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_641(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optPropertyParms(646=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_647(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_optSep(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(83=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(85, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(88, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(107=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(444, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(88, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(451, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(88, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(475, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(477, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(476, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_496(496, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(480, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(477, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(497, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(498, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(783, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(477, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(787, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(477, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(961=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_965(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optSep(962=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_964(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_packagesDescriptor(898=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_902(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_packagesDescriptor(927=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_902(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_packagesDescriptor(939=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_902(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_packagesItem(315, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_317(317, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_packagesItem(919, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_920(920, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_packagesItem(922, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_923(923, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_packagesItems(920, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_921(921, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_packagesItems(923=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_924(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_parmValue(243=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_284(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parmValue(334=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parmValue(355=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parmValue(430=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_457(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parmValue(489=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_490(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parmValue(530=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parmValue(551=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_pathName(88=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_960(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_pkgdName(220, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_222(222, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(225=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(332, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_334(334, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(353, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_355(355, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(370, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_355(355, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(380=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_381(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(385=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_381(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(388, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_389(389, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(477, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_484(484, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(498, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_499(499, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(506=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_508(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(527, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_530(530, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(543=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_508(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(549, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(551, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(557, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(551, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(591, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_593(593, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(601, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_593(593, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(605=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(612=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(615=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(620=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(642, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(551, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(660, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(551, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(674, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(551, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(687, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(551, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(700, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(551, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(711, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_713(713, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(740, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_742(742, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(762, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_742(742, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pkgdName(779, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_713(713, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_portNumber(99, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(101, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_portNumber(106, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(107, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_portNumber(444=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_445(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_priority(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_priority(506=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_507(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_priority(527=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_507(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_priority(543=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_507(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_priority(566=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_priority(832=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_priority(943=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_propertyParm(332=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_333(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(353=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_354(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(370=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_354(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(527, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_529(529, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(549, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_550(550, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(557, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_529(529, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(642, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_643(643, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(660=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_663(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(674=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_663(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(687=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_688(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParm(700=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_688(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_propertyParmList(529=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_548(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParmList(550=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_552(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParmList(643, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_644(644, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_propertyParms(527, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_528(528, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_propertyParms(557, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_528(528, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_requestID(259=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_261(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_requestID(379, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_384(384, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_requestID(473, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_474(474, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_requestID(709, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_710(710, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_requestID(738, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_739(739, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_requestedEvent(711, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_712(712, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_requestedEvent(779, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_780(780, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_requestedEventBody(713=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_714(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_requestedEvents(712, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_778(778, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_requestedEvents(780=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_781(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_safeToken(4, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(85, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(6, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(7, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(80, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(81, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(82, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(83, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(88=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_959(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(92, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(104, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(93, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(94, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(94, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(94, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(96, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(94, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(99=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(106=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(124=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_802(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(128=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(185=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(188=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(220=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(225=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(233=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_235(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(241, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(243, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(252=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(259=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_260(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(275=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_276(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(277=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_278(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(285=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_290(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(286=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_290(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(287=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_290(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(288=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_290(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(296=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_290(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(297=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_290(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(300=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_290(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(301=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_290(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(311, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(243, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(315=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_316(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(332=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(348=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(353=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(370=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(379=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_260(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(380=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(385=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(388=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(393=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_394(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(414=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(419=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_429(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(442=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_443(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(444=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(447=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_290(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(449=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_450(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(453=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_454(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(455=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_456(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_429(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(464=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_465(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(466=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(473=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_260(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(477=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(486=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_394(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(492=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_394(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(498=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(506=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(527=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(535=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(538=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(543=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(549=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(557=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(558=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(561=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(569=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(591=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(594=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_290(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(596=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_290(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(601=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(605=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(610=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_235(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(612=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(615=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(620=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(624=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_625(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(628=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(632=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_634(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(633=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_634(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(637=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_634(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(642=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(660=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(674=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(678=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(687=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(700=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(709=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_260(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(711=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(715=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_394(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(738=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_260(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(740=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(744=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_394(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(757=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_394(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(762=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(774=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_394(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(779=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(811=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_813(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(815=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_813(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(819=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_821(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(830=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(850=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(875=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(881=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(887=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(890=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_891(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(919=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_316(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(922=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_316(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(931=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(936=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(952=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_802(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_safeToken(956=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_821(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_secondEventParameter(744, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_745(745, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_secondEventParameter(757, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_758(758, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_secondEventParameters(745, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_756(756, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_secondEventParameters(758=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_759(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_secondRequestedEvent(740, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_741(741, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_secondRequestedEvent(762, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_763(763, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_secondRequestedEventBody(742=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_743(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_secondRequestedEvents(741, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_761(761, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_secondRequestedEvents(763=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_764(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_segmentReply(89=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_segmentReply(114=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_servChgReplyParm(857, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_863(863, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_servChgReplyParm(869, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_870(870, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_servChgReplyParms(863, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_868(868, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_servChgReplyParms(870=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_871(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeAddress(419=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_428(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeAddress(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_428(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeAddress(857=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_862(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeAddress(869=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_862(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeDelay(419=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_427(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeDelay(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_427(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeDescriptor(416, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_417(417, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeMethod(419=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_426(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeMethod(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_426(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeMgcId(419=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_425(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeMgcId(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_425(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeMgcId(857=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_861(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeMgcId(869=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_861(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeParm(419, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_424(424, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeParm(459, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_460(460, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeParms(424, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_458(458, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeParms(460=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_461(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeProfile(419=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_423(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeProfile(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_423(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeProfile(857=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_860(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeProfile(869=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_860(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeReason(419=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_422(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeReason(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_422(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeReply(832=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_833(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeReply(943=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_833(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeReplyBody(851=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_852(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeReplyDescriptor(853, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_854(854, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeRequest(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeRequest(566=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceChangeVersion(419=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_421(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeVersion(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_421(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeVersion(857=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_859(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceChangeVersion(869=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_859(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceStates(660=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_662(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceStates(674=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_662(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_serviceStatesValue(339=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceStatesValue(340=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_341(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_serviceStatesValue(667=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_668(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_sigParameter(241, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_242(242, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sigParameter(311, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_312(312, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_sigParameters(242, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_310(310, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sigParameters(312=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_313(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalList(605=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_608(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalList(620=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_608(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalListId(233, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(234, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalListId(610, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_611(611, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalListParm(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_238(238, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalListParm(612, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_613(613, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalListParm(615, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_616(616, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalListParms(613, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_614(614, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalListParms(616=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_617(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalName(225, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(227, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalName(236, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(227, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalName(605, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(227, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalName(612, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(227, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalName(615, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(227, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalName(620, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(227, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalParm(605, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_607(607, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalParm(620, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_621(621, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalParms(607, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_619(619, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalParms(621=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_622(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalRequest(225=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalRequest(236=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_237(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalRequest(605=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_606(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalRequest(612=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_237(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalRequest(615=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_237(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalRequest(620=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_606(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalType(254=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_255(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signalsDescriptor(572=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_574(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalsDescriptor(734, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_735(735, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalsDescriptor(753, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_754(754, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalsDescriptor(792=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_574(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalsDescriptor(898=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_901(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalsDescriptor(927=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_901(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signalsDescriptor(939=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_901(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_statisticsDescriptor(572=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_573(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsDescriptor(648=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_652(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsDescriptor(680=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_652(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsDescriptor(683=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_652(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsDescriptor(705=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_652(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsDescriptor(792=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_573(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsDescriptor(898=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_900(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsDescriptor(927=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_900(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsDescriptor(939=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_900(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_statisticsParameter(591, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_592(592, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsParameter(601, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_602(602, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_statisticsParameters(592, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_600(600, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_statisticsParameters(602=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_603(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_streamDescriptor(648=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_651(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamDescriptor(705=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_651(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_streamID(174=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamID(252=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_253(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamID(348, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(349, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamID(678, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_679(679, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_streamModes(360=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_368(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamModes(361=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_362(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamModes(697=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_698(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_streamParm(648=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_650(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamParm(680, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_681(681, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamParm(683, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_684(684, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamParm(705=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_650(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_streamParmList(681, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_682(682, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_streamParmList(684=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_685(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_subtractRequest(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_subtractRequest(566=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_termIDList(182, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(184, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termIDList(414, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_415(415, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termIDList(466, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_467(467, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termIDList(558, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_559(559, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termIDList(561, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_562(562, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_termIDList(569, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_570(570, Cat, Ss, Stack, T, Ts, Tzr);
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

yeccgoto_terminationID(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(182=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(185, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_186(186, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(188, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(189, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(414=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(466=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(558=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(561=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(569=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(628, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_629(629, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(850=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(875=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(881=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(887, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_629(629, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(931=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationID(936=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_183(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_terminationIDList(626=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_627(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationIDList(884=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_885(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationIDList(933=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_885(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_terminationIDListRepeat(186, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_187(187, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationIDListRepeat(189=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_190(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationIDListRepeat(629, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_630(630, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_terminationStateDescriptor(648=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_649(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationStateDescriptor(705=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_649(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_terminationStateParm(660, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_661(661, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationStateParm(674, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_675(675, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_terminationStateParms(661, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_673(673, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_terminationStateParms(675=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_676(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_timeStamp(419=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_420(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_timeStamp(459=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_420(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_timeStamp(475, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_476(476, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_timeStamp(480, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_476(476, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_timeStamp(783, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_476(476, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_timeStamp(787, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_476(476, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_timeStamp(857=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_858(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_timeStamp(869=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_858(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_topologyDescComp(162, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_164(164, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_topologyDescComp(178, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(179, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_topologyDescCompList(164, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(177, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_topologyDescCompList(179=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_topologyDescriptor(131=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_topologyDescriptor(566=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_topologyDescriptor(832=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_topologyDescriptor(943=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_topologyDirection(162=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_topologyDirection(178=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionAck(811, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_812(812, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionAck(815, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_816(816, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionAckList(812, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_814(814, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionAckList(816=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_817(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionID(124, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_801(801, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionID(952, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_953(953, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionID_and_segment_info(819, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_820(820, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionID_and_segment_info(956=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_957(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionItem(89, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionItem(114, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(114, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionList(89=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionList(114=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_958(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionPending(89=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionPending(114=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionReply(89=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionReply(114=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionReplyBody(823, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_826(826, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionRequest(89=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionRequest(114=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_transactionResponseAck(89=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_transactionResponseAck(114=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_value(285=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_294(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(286=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_293(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(287=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_292(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(288=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_289(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(296, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_307(307, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(297, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_298(298, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(300, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_304(304, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(301, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_302(302, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(447=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_448(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(594=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_595(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(596, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_597(597, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_valueList(298, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_299(299, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_valueList(302=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_303(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_valueList(307, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_308(308, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_valueList(597, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_598(598, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,{yeccpars2_0_,1}}).
-file("megaco_text_parser_v3.yrl", 484).
yeccpars2_0_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_1_,1}}).
-file("megaco_text_parser_v3.yrl", 489).
yeccpars2_1_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_3_,1}}).
-file("megaco_text_parser_v3.yrl", 483).
yeccpars2_3_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   sep
  end | __Stack].

-compile({inline,{yeccpars2_8_,1}}).
-file("megaco_text_parser_v3.yrl", 1547).
yeccpars2_8_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_9_,1}}).
-file("megaco_text_parser_v3.yrl", 1546).
yeccpars2_9_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_10_,1}}).
-file("megaco_text_parser_v3.yrl", 1548).
yeccpars2_10_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_11_,1}}).
-file("megaco_text_parser_v3.yrl", 1549).
yeccpars2_11_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_12_,1}}).
-file("megaco_text_parser_v3.yrl", 1552).
yeccpars2_12_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_13_,1}}).
-file("megaco_text_parser_v3.yrl", 1556).
yeccpars2_13_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_14_,1}}).
-file("megaco_text_parser_v3.yrl", 1554).
yeccpars2_14_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_15_,1}}).
-file("megaco_text_parser_v3.yrl", 1564).
yeccpars2_15_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_16_,1}}).
-file("megaco_text_parser_v3.yrl", 1561).
yeccpars2_16_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_17_,1}}).
-file("megaco_text_parser_v3.yrl", 1562).
yeccpars2_17_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_18_,1}}).
-file("megaco_text_parser_v3.yrl", 1563).
yeccpars2_18_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_19_,1}}).
-file("megaco_text_parser_v3.yrl", 1565).
yeccpars2_19_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_20_,1}}).
-file("megaco_text_parser_v3.yrl", 1566).
yeccpars2_20_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_21_,1}}).
-file("megaco_text_parser_v3.yrl", 1569).
yeccpars2_21_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_22_,1}}).
-file("megaco_text_parser_v3.yrl", 1573).
yeccpars2_22_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_23_,1}}).
-file("megaco_text_parser_v3.yrl", 1574).
yeccpars2_23_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_24_,1}}).
-file("megaco_text_parser_v3.yrl", 1575).
yeccpars2_24_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_25_,1}}).
-file("megaco_text_parser_v3.yrl", 1576).
yeccpars2_25_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_26_,1}}).
-file("megaco_text_parser_v3.yrl", 1577).
yeccpars2_26_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_27_,1}}).
-file("megaco_text_parser_v3.yrl", 1578).
yeccpars2_27_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_28_,1}}).
-file("megaco_text_parser_v3.yrl", 1579).
yeccpars2_28_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_29_,1}}).
-file("megaco_text_parser_v3.yrl", 1581).
yeccpars2_29_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_30_,1}}).
-file("megaco_text_parser_v3.yrl", 1587).
yeccpars2_30_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_31_,1}}).
-file("megaco_text_parser_v3.yrl", 1582).
yeccpars2_31_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_32_,1}}).
-file("megaco_text_parser_v3.yrl", 1584).
yeccpars2_32_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_33_,1}}).
-file("megaco_text_parser_v3.yrl", 1585).
yeccpars2_33_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_34_,1}}).
-file("megaco_text_parser_v3.yrl", 1588).
yeccpars2_34_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_35_,1}}).
-file("megaco_text_parser_v3.yrl", 1591).
yeccpars2_35_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_36_,1}}).
-file("megaco_text_parser_v3.yrl", 1593).
yeccpars2_36_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_37_,1}}).
-file("megaco_text_parser_v3.yrl", 1592).
yeccpars2_37_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_38_,1}}).
-file("megaco_text_parser_v3.yrl", 1596).
yeccpars2_38_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_39_,1}}).
-file("megaco_text_parser_v3.yrl", 1597).
yeccpars2_39_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_40_,1}}).
-file("megaco_text_parser_v3.yrl", 1606).
yeccpars2_40_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_41_,1}}).
-file("megaco_text_parser_v3.yrl", 1605).
yeccpars2_41_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_42_,1}}).
-file("megaco_text_parser_v3.yrl", 1607).
yeccpars2_42_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_43_,1}}).
-file("megaco_text_parser_v3.yrl", 1612).
yeccpars2_43_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_44_,1}}).
-file("megaco_text_parser_v3.yrl", 1614).
yeccpars2_44_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_45_,1}}).
-file("megaco_text_parser_v3.yrl", 1613).
yeccpars2_45_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_46_,1}}).
-file("megaco_text_parser_v3.yrl", 1616).
yeccpars2_46_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_47_,1}}).
-file("megaco_text_parser_v3.yrl", 1615).
yeccpars2_47_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_48_,1}}).
-file("megaco_text_parser_v3.yrl", 1618).
yeccpars2_48_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_49_,1}}).
-file("megaco_text_parser_v3.yrl", 1620).
yeccpars2_49_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_50_,1}}).
-file("megaco_text_parser_v3.yrl", 1621).
yeccpars2_50_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_51_,1}}).
-file("megaco_text_parser_v3.yrl", 1622).
yeccpars2_51_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_52_,1}}).
-file("megaco_text_parser_v3.yrl", 1623).
yeccpars2_52_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_53_,1}}).
-file("megaco_text_parser_v3.yrl", 1625).
yeccpars2_53_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_54_,1}}).
-file("megaco_text_parser_v3.yrl", 1626).
yeccpars2_54_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_55_,1}}).
-file("megaco_text_parser_v3.yrl", 1627).
yeccpars2_55_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_56_,1}}).
-file("megaco_text_parser_v3.yrl", 1544).
yeccpars2_56_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_57_,1}}).
-file("megaco_text_parser_v3.yrl", 1632).
yeccpars2_57_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_58_,1}}).
-file("megaco_text_parser_v3.yrl", 1633).
yeccpars2_58_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_59_,1}}).
-file("megaco_text_parser_v3.yrl", 1638).
yeccpars2_59_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_60_,1}}).
-file("megaco_text_parser_v3.yrl", 1636).
yeccpars2_60_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_61_,1}}).
-file("megaco_text_parser_v3.yrl", 1634).
yeccpars2_61_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_62_,1}}).
-file("megaco_text_parser_v3.yrl", 1639).
yeccpars2_62_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_63_,1}}).
-file("megaco_text_parser_v3.yrl", 1641).
yeccpars2_63_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_64_,1}}).
-file("megaco_text_parser_v3.yrl", 1643).
yeccpars2_64_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_65_,1}}).
-file("megaco_text_parser_v3.yrl", 1645).
yeccpars2_65_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_66_,1}}).
-file("megaco_text_parser_v3.yrl", 1646).
yeccpars2_66_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_67_,1}}).
-file("megaco_text_parser_v3.yrl", 1647).
yeccpars2_67_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_68_,1}}).
-file("megaco_text_parser_v3.yrl", 1648).
yeccpars2_68_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_69_,1}}).
-file("megaco_text_parser_v3.yrl", 1650).
yeccpars2_69_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_70_,1}}).
-file("megaco_text_parser_v3.yrl", 1651).
yeccpars2_70_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_71_,1}}).
-file("megaco_text_parser_v3.yrl", 1652).
yeccpars2_71_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_72_,1}}).
-file("megaco_text_parser_v3.yrl", 1653).
yeccpars2_72_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_73_,1}}).
-file("megaco_text_parser_v3.yrl", 1654).
yeccpars2_73_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_74_,1}}).
-file("megaco_text_parser_v3.yrl", 1655).
yeccpars2_74_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_75_,1}}).
-file("megaco_text_parser_v3.yrl", 1656).
yeccpars2_75_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_76_,1}}).
-file("megaco_text_parser_v3.yrl", 1657).
yeccpars2_76_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_77_,1}}).
-file("megaco_text_parser_v3.yrl", 1658).
yeccpars2_77_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_78_,1}}).
-file("megaco_text_parser_v3.yrl", 1659).
yeccpars2_78_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_79_,1}}).
-file("megaco_text_parser_v3.yrl", 1660).
yeccpars2_79_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_83_,1}}).
-file("megaco_text_parser_v3.yrl", 484).
yeccpars2_83_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_84_,1}}).
-file("megaco_text_parser_v3.yrl", 488).
yeccpars2_84_(__Stack0) ->
 [__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_auth_header ( __3 , __5 , __7 )
  end | __Stack].

-compile({inline,{yeccpars2_85_,1}}).
-file("megaco_text_parser_v3.yrl", 484).
yeccpars2_85_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_87_,1}}).
-file("megaco_text_parser_v3.yrl", 481).
yeccpars2_87_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'MegacoMessage' { authHeader = __2 , mess = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_93_,1}}).
-file("megaco_text_parser_v3.yrl", 1023).
yeccpars2_93_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_94_,1}}).
-file("megaco_text_parser_v3.yrl", 1023).
yeccpars2_94_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_96_,1}}).
-file("megaco_text_parser_v3.yrl", 1023).
yeccpars2_96_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_97_,1}}).
-file("megaco_text_parser_v3.yrl", 1024).
yeccpars2_97_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ colon | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_98_,1}}).
-file("megaco_text_parser_v3.yrl", 1021).
yeccpars2_98_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_domainAddress ( __2 , asn1_NOVALUE )
  end | __Stack].

-compile({inline,{yeccpars2_100_,1}}).
-file("megaco_text_parser_v3.yrl", 1028).
yeccpars2_100_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_uint16 ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_101_,1}}).
-file("megaco_text_parser_v3.yrl", 484).
yeccpars2_101_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_102_,1}}).
-file("megaco_text_parser_v3.yrl", 1019).
yeccpars2_102_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_domainAddress ( __2 , __5 )
  end | __Stack].

-compile({inline,{yeccpars2_103_,1}}).
-file("megaco_text_parser_v3.yrl", 1025).
yeccpars2_103_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_105_,1}}).
-file("megaco_text_parser_v3.yrl", 1011).
yeccpars2_105_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_domainName ( __2 , asn1_NOVALUE )
  end | __Stack].

-compile({inline,{yeccpars2_107_,1}}).
-file("megaco_text_parser_v3.yrl", 484).
yeccpars2_107_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_108_,1}}).
-file("megaco_text_parser_v3.yrl", 1009).
yeccpars2_108_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_domainName ( __2 , __5 )
  end | __Stack].

-compile({inline,{yeccpars2_109_,1}}).
-file("megaco_text_parser_v3.yrl", 503).
yeccpars2_109_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { transactionResponseAck , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_110_,1}}).
-file("megaco_text_parser_v3.yrl", 500).
yeccpars2_110_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { transactionRequest , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_111_,1}}).
-file("megaco_text_parser_v3.yrl", 501).
yeccpars2_111_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { transactionReply , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_112_,1}}).
-file("megaco_text_parser_v3.yrl", 502).
yeccpars2_112_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { transactionPending , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_113_,1}}).
-file("megaco_text_parser_v3.yrl", 495).
yeccpars2_113_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { transactions , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_114_,1}}).
-file("megaco_text_parser_v3.yrl", 497).
yeccpars2_114_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_115_,1}}).
-file("megaco_text_parser_v3.yrl", 504).
yeccpars2_115_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { segmentReply , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_116_,1}}).
-file("megaco_text_parser_v3.yrl", 492).
yeccpars2_116_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_message ( __1 , __2 , __3 )
  end | __Stack].

-compile({inline,{yeccpars2_117_,1}}).
-file("megaco_text_parser_v3.yrl", 494).
yeccpars2_117_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { messageError , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_126_,1}}).
-file("megaco_text_parser_v3.yrl", 533).
yeccpars2_126_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_129_,1}}).
-file("megaco_text_parser_v3.yrl", 1016).
yeccpars2_129_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_contextID ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_132_,1}}).
-file("megaco_text_parser_v3.yrl", 558).
yeccpars2_132_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { topology , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_135_,1}}).
-file("megaco_text_parser_v3.yrl", 559).
yeccpars2_135_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { priority , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_137_,1}}).
-file("megaco_text_parser_v3.yrl", 562).
yeccpars2_137_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { iepsCallind , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_138_,1}}).
-file("megaco_text_parser_v3.yrl", 552).
yeccpars2_138_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { contextProp , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_139_,1}}).
-file("megaco_text_parser_v3.yrl", 553).
yeccpars2_139_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { contextAudit , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_141_,1}}).
-file("megaco_text_parser_v3.yrl", 554).
yeccpars2_141_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { commandRequest , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_145_,1}}).
-file("megaco_text_parser_v3.yrl", 550).
yeccpars2_145_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_147_,1}}).
-file("megaco_text_parser_v3.yrl", 673).
yeccpars2_147_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { addReq , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_152_,1}}).
-file("megaco_text_parser_v3.yrl", 561).
yeccpars2_152_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { emergency , false }
  end | __Stack].

-compile({inline,{yeccpars2_153_,1}}).
-file("megaco_text_parser_v3.yrl", 560).
yeccpars2_153_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { emergency , true }
  end | __Stack].

-compile({inline,{yeccpars2_155_,1}}).
-file("megaco_text_parser_v3.yrl", 675).
yeccpars2_155_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { modReq , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_156_,1}}).
-file("megaco_text_parser_v3.yrl", 674).
yeccpars2_156_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { moveReq , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_164_,1}}).
-file("megaco_text_parser_v3.yrl", 1522).
yeccpars2_164_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_165_,1}}).
-file("megaco_text_parser_v3.yrl", 1518).
yeccpars2_165_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { tid , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_166_,1}}).
-file("megaco_text_parser_v3.yrl", 1046).
yeccpars2_166_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_terminationID ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_167_,1}}).
-file("megaco_text_parser_v3.yrl", 1519).
yeccpars2_167_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { sid , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_168_,1}}).
-file("megaco_text_parser_v3.yrl", 1526).
yeccpars2_168_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { direction , bothway }
  end | __Stack].

-compile({inline,{yeccpars2_169_,1}}).
-file("megaco_text_parser_v3.yrl", 1527).
yeccpars2_169_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { direction , isolate }
  end | __Stack].

-compile({inline,{yeccpars2_170_,1}}).
-file("megaco_text_parser_v3.yrl", 1530).
yeccpars2_170_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { direction_ext , onewayboth }
  end | __Stack].

-compile({inline,{yeccpars2_171_,1}}).
-file("megaco_text_parser_v3.yrl", 1529).
yeccpars2_171_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { direction_ext , onewayexternal }
  end | __Stack].

-compile({inline,{yeccpars2_172_,1}}).
-file("megaco_text_parser_v3.yrl", 1528).
yeccpars2_172_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { direction , oneway }
  end | __Stack].

-compile({inline,{yeccpars2_173_,1}}).
-file("megaco_text_parser_v3.yrl", 1643).
yeccpars2_173_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_175_,1}}).
-file("megaco_text_parser_v3.yrl", 946).
yeccpars2_175_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_176_,1}}).
-file("megaco_text_parser_v3.yrl", 1183).
yeccpars2_176_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_streamID ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_179_,1}}).
-file("megaco_text_parser_v3.yrl", 1522).
yeccpars2_179_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_180_,1}}).
-file("megaco_text_parser_v3.yrl", 1524).
yeccpars2_180_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_181_,1}}).
-file("megaco_text_parser_v3.yrl", 1516).
yeccpars2_181_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_topologyDescriptor ( [ __3 | __4 ] )
  end | __Stack].

-compile({inline,{yeccpars2_183_,1}}).
-file("megaco_text_parser_v3.yrl", 1032).
yeccpars2_183_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_184_,1}}).
-file("megaco_text_parser_v3.yrl", 713).
yeccpars2_184_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_186_,1}}).
-file("megaco_text_parser_v3.yrl", 1041).
yeccpars2_186_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_189_,1}}).
-file("megaco_text_parser_v3.yrl", 1041).
yeccpars2_189_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_190_,1}}).
-file("megaco_text_parser_v3.yrl", 1040).
yeccpars2_190_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_191_,1}}).
-file("megaco_text_parser_v3.yrl", 1034).
yeccpars2_191_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_192_,1}}).
-file("megaco_text_parser_v3.yrl", 707).
yeccpars2_192_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   SR = # 'SubtractRequest' { terminationID = __3 ,
    auditDescriptor = __4 } ,
    make_commandRequest ( { subtractReq , __1 } , SR )
  end | __Stack].

-compile({inline,{yeccpars2_196_,1}}).
-file("megaco_text_parser_v3.yrl", 765).
yeccpars2_196_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_197_,1}}).
-file("megaco_text_parser_v3.yrl", 786).
yeccpars2_197_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { terminationAudit , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_198_,1}}).
-file("megaco_text_parser_v3.yrl", 813).
yeccpars2_198_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { indAudStatisticsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_199_,1}}).
-file("megaco_text_parser_v3.yrl", 807).
yeccpars2_199_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { indAudSignalsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_200_,1}}).
-file("megaco_text_parser_v3.yrl", 815).
yeccpars2_200_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { indAudPackagesDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_201_,1}}).
-file("megaco_text_parser_v3.yrl", 803).
yeccpars2_201_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { indAudMediaDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_202_,1}}).
-file("megaco_text_parser_v3.yrl", 805).
yeccpars2_202_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { indAudEventsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_203_,1}}).
-file("megaco_text_parser_v3.yrl", 811).
yeccpars2_203_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { indAudEventBufferDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_204_,1}}).
-file("megaco_text_parser_v3.yrl", 809).
yeccpars2_204_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { indAudDigitMapDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_205_,1}}).
-file("megaco_text_parser_v3.yrl", 800).
yeccpars2_205_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_207_,1}}).
-file("megaco_text_parser_v3.yrl", 768).
yeccpars2_207_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_209_,1}}).
-file("megaco_text_parser_v3.yrl", 938).
yeccpars2_209_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_IADMD ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_210_,1}}).
-file("megaco_text_parser_v3.yrl", 775).
yeccpars2_210_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   digitMapToken
  end | __Stack].

-compile({inline,{yeccpars2_211_,1}}).
-file("megaco_text_parser_v3.yrl", 784).
yeccpars2_211_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   eventBufferToken
  end | __Stack].

-compile({inline,{yeccpars2_212_,1}}).
-file("megaco_text_parser_v3.yrl", 785).
yeccpars2_212_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   eventsToken
  end | __Stack].

-compile({inline,{yeccpars2_213_,1}}).
-file("megaco_text_parser_v3.yrl", 774).
yeccpars2_213_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   mediaToken
  end | __Stack].

-compile({inline,{yeccpars2_214_,1}}).
-file("megaco_text_parser_v3.yrl", 773).
yeccpars2_214_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   modemToken
  end | __Stack].

-compile({inline,{yeccpars2_215_,1}}).
-file("megaco_text_parser_v3.yrl", 772).
yeccpars2_215_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   muxToken
  end | __Stack].

-compile({inline,{yeccpars2_216_,1}}).
-file("megaco_text_parser_v3.yrl", 777).
yeccpars2_216_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   observedEventsToken
  end | __Stack].

-compile({inline,{yeccpars2_217_,1}}).
-file("megaco_text_parser_v3.yrl", 778).
yeccpars2_217_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   packagesToken
  end | __Stack].

-compile({inline,{yeccpars2_218_,1}}).
-file("megaco_text_parser_v3.yrl", 783).
yeccpars2_218_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   signalsToken
  end | __Stack].

-compile({inline,{yeccpars2_219_,1}}).
-file("megaco_text_parser_v3.yrl", 776).
yeccpars2_219_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   statsToken
  end | __Stack].

-compile({inline,{yeccpars2_221_,1}}).
-file("megaco_text_parser_v3.yrl", 1185).
yeccpars2_221_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_pkgdName ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_223_,1}}).
-file("megaco_text_parser_v3.yrl", 941).
yeccpars2_223_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'IndAudStatisticsDescriptor' { statName = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_224_,1}}).
-file("megaco_text_parser_v3.yrl", 918).
yeccpars2_224_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_226_,1}}).
-file("megaco_text_parser_v3.yrl", 925).
yeccpars2_226_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { signal , ensure_indAudSignal ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_227_,1}}).
-file("megaco_text_parser_v3.yrl", 1294).
yeccpars2_227_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   merge_signalRequest ( __1 , [ ] )
  end | __Stack].

-compile({inline,{yeccpars2_230_,1}}).
-file("megaco_text_parser_v3.yrl", 924).
yeccpars2_230_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { seqSigList , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_231_,1}}).
-file("megaco_text_parser_v3.yrl", 921).
yeccpars2_231_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   asn1_NOVALUE
  end | __Stack].

-compile({inline,{yeccpars2_232_,1}}).
-file("megaco_text_parser_v3.yrl", 1639).
yeccpars2_232_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_234_,1}}).
-file("megaco_text_parser_v3.yrl", 928).
yeccpars2_234_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'IndAudSeqSigList' { id = ensure_uint16 ( __3 ) }
  end | __Stack].

-compile({inline,{yeccpars2_235_,1}}).
-file("megaco_text_parser_v3.yrl", 1364).
yeccpars2_235_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_uint16 ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_239_,1}}).
-file("megaco_text_parser_v3.yrl", 931).
yeccpars2_239_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'IndAudSeqSigList' { id = ensure_uint16 ( __3 ) ,
    signalList =
    ensure_indAudSignalListParm ( __5 ) }
  end | __Stack].

-compile({inline,{yeccpars2_240_,1}}).
-file("megaco_text_parser_v3.yrl", 922).
yeccpars2_240_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_242_,1}}).
-file("megaco_text_parser_v3.yrl", 1297).
yeccpars2_242_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_244_,1}}).
-file("megaco_text_parser_v3.yrl", 1561).
yeccpars2_244_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_245_,1}}).
-file("megaco_text_parser_v3.yrl", 1565).
yeccpars2_245_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_247_COMMA,1}}).
-file("megaco_text_parser_v3.yrl", 1328).
yeccpars2_247_COMMA(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   keepActive
  end | __Stack].

-compile({inline,{yeccpars2_247_RBRKT,1}}).
-file("megaco_text_parser_v3.yrl", 1328).
yeccpars2_247_RBRKT(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   keepActive
  end | __Stack].

-compile({inline,{yeccpars2_247_,1}}).
-file("megaco_text_parser_v3.yrl", 1588).
yeccpars2_247_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_248_,1}}).
-file("megaco_text_parser_v3.yrl", 1606).
yeccpars2_248_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_249_,1}}).
-file("megaco_text_parser_v3.yrl", 1625).
yeccpars2_249_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_250_,1}}).
-file("megaco_text_parser_v3.yrl", 1641).
yeccpars2_250_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_251_,1}}).
-file("megaco_text_parser_v3.yrl", 1643).
yeccpars2_251_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_253_,1}}).
-file("megaco_text_parser_v3.yrl", 1320).
yeccpars2_253_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { stream , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_255_,1}}).
-file("megaco_text_parser_v3.yrl", 1322).
yeccpars2_255_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { signal_type , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_256_,1}}).
-file("megaco_text_parser_v3.yrl", 1340).
yeccpars2_256_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   brief
  end | __Stack].

-compile({inline,{yeccpars2_257_,1}}).
-file("megaco_text_parser_v3.yrl", 1338).
yeccpars2_257_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   onOff
  end | __Stack].

-compile({inline,{yeccpars2_258_,1}}).
-file("megaco_text_parser_v3.yrl", 1339).
yeccpars2_258_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   timeOut
  end | __Stack].

-compile({inline,{yeccpars2_260_,1}}).
-file("megaco_text_parser_v3.yrl", 1398).
yeccpars2_260_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_requestID ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_261_,1}}).
-file("megaco_text_parser_v3.yrl", 1332).
yeccpars2_261_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { requestId , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_264_,1}}).
-file("megaco_text_parser_v3.yrl", 1347).
yeccpars2_264_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_265_,1}}).
-file("megaco_text_parser_v3.yrl", 1350).
yeccpars2_265_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   onInterruptByEvent
  end | __Stack].

-compile({inline,{yeccpars2_266_,1}}).
-file("megaco_text_parser_v3.yrl", 1351).
yeccpars2_266_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   onInterruptByNewSignalDescr
  end | __Stack].

-compile({inline,{yeccpars2_267_,1}}).
-file("megaco_text_parser_v3.yrl", 1353).
yeccpars2_267_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   iteration
  end | __Stack].

-compile({inline,{yeccpars2_268_,1}}).
-file("megaco_text_parser_v3.yrl", 1352).
yeccpars2_268_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   otherReason
  end | __Stack].

-compile({inline,{yeccpars2_269_,1}}).
-file("megaco_text_parser_v3.yrl", 1349).
yeccpars2_269_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   onTimeOut
  end | __Stack].

-compile({inline,{yeccpars2_272_,1}}).
-file("megaco_text_parser_v3.yrl", 1347).
yeccpars2_272_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_273_,1}}).
-file("megaco_text_parser_v3.yrl", 1346).
yeccpars2_273_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_274_,1}}).
-file("megaco_text_parser_v3.yrl", 1327).
yeccpars2_274_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { notify_completion , [ __4 | __5 ] }
  end | __Stack].

-compile({inline,{yeccpars2_276_,1}}).
-file("megaco_text_parser_v3.yrl", 1334).
yeccpars2_276_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { intersigDelay , ensure_uint16 ( __3 ) }
  end | __Stack].

-compile({inline,{yeccpars2_278_,1}}).
-file("megaco_text_parser_v3.yrl", 1324).
yeccpars2_278_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { duration , ensure_uint16 ( __3 ) }
  end | __Stack].

-compile({inline,{yeccpars2_280_,1}}).
-file("megaco_text_parser_v3.yrl", 1330).
yeccpars2_280_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { direction , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_281_,1}}).
-file("megaco_text_parser_v3.yrl", 1344).
yeccpars2_281_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   both
  end | __Stack].

-compile({inline,{yeccpars2_282_,1}}).
-file("megaco_text_parser_v3.yrl", 1342).
yeccpars2_282_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   external
  end | __Stack].

-compile({inline,{yeccpars2_283_,1}}).
-file("megaco_text_parser_v3.yrl", 1343).
yeccpars2_283_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   internal
  end | __Stack].

-compile({inline,{yeccpars2_284_,1}}).
-file("megaco_text_parser_v3.yrl", 1336).
yeccpars2_284_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { other , ensure_NAME ( __1 ) , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_289_,1}}).
-file("megaco_text_parser_v3.yrl", 1118).
yeccpars2_289_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   # 'PropertyParm' { value = [ __2 ] ,
    extraInfo = { relation , unequalTo } }
  end | __Stack].

-compile({inline,{yeccpars2_290_,1}}).
-file("megaco_text_parser_v3.yrl", 1542).
yeccpars2_290_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_value ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_291_,1}}).
-file("megaco_text_parser_v3.yrl", 1541).
yeccpars2_291_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_value ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_292_,1}}).
-file("megaco_text_parser_v3.yrl", 1121).
yeccpars2_292_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   # 'PropertyParm' { value = [ __2 ] ,
    extraInfo = { relation , smallerThan } }
  end | __Stack].

-compile({inline,{yeccpars2_293_,1}}).
-file("megaco_text_parser_v3.yrl", 1124).
yeccpars2_293_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   # 'PropertyParm' { value = [ __2 ] ,
    extraInfo = { relation , greaterThan } }
  end | __Stack].

-compile({inline,{yeccpars2_294_,1}}).
-file("megaco_text_parser_v3.yrl", 1145).
yeccpars2_294_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'PropertyParm' { value = [ __1 ] }
  end | __Stack].

-compile({inline,{yeccpars2_295_,1}}).
-file("megaco_text_parser_v3.yrl", 1115).
yeccpars2_295_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_298_,1}}).
-file("megaco_text_parser_v3.yrl", 1148).
yeccpars2_298_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_302_,1}}).
-file("megaco_text_parser_v3.yrl", 1148).
yeccpars2_302_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_303_,1}}).
-file("megaco_text_parser_v3.yrl", 1147).
yeccpars2_303_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_305_,1}}).
-file("megaco_text_parser_v3.yrl", 1137).
yeccpars2_305_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'PropertyParm' { value = [ __2 , __4 ] ,
    extraInfo = { range , true } }
  end | __Stack].

-compile({inline,{yeccpars2_306_,1}}).
-file("megaco_text_parser_v3.yrl", 1141).
yeccpars2_306_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'PropertyParm' { value = [ __2 | __3 ] ,
    extraInfo = { sublist , true } }
  end | __Stack].

-compile({inline,{yeccpars2_307_,1}}).
-file("megaco_text_parser_v3.yrl", 1148).
yeccpars2_307_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_309_,1}}).
-file("megaco_text_parser_v3.yrl", 1133).
yeccpars2_309_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'PropertyParm' { value = [ __2 | __3 ] ,
    extraInfo = { sublist , false } }
  end | __Stack].

-compile({inline,{yeccpars2_312_,1}}).
-file("megaco_text_parser_v3.yrl", 1297).
yeccpars2_312_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_313_,1}}).
-file("megaco_text_parser_v3.yrl", 1296).
yeccpars2_313_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_314_,1}}).
-file("megaco_text_parser_v3.yrl", 1293).
yeccpars2_314_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_signalRequest ( __1 , [ __3 | __4 ] )
  end | __Stack].

-compile({inline,{yeccpars2_316_,1}}).
-file("megaco_text_parser_v3.yrl", 1490).
yeccpars2_316_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_packagesItem ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_318_,1}}).
-file("megaco_text_parser_v3.yrl", 944).
yeccpars2_318_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_indAudPackagesDescriptor ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_320_,1}}).
-file("megaco_text_parser_v3.yrl", 828).
yeccpars2_320_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { termStateDescr , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_321_,1}}).
-file("megaco_text_parser_v3.yrl", 826).
yeccpars2_321_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { streamParm , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_322_,1}}).
-file("megaco_text_parser_v3.yrl", 827).
yeccpars2_322_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { streamDescr , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_323_,1}}).
-file("megaco_text_parser_v3.yrl", 843).
yeccpars2_323_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'IndAudStreamParms' { statisticsDescriptor = __1 }
  end | __Stack].

-compile({inline,{yeccpars2_324_,1}}).
-file("megaco_text_parser_v3.yrl", 831).
yeccpars2_324_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_325_,1}}).
-file("megaco_text_parser_v3.yrl", 841).
yeccpars2_325_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'IndAudStreamParms' { localControlDescriptor = __1 }
  end | __Stack].

-compile({inline,{yeccpars2_327_,1}}).
-file("megaco_text_parser_v3.yrl", 838).
yeccpars2_327_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   LD = ensure_prop_groups ( __1 ) ,
    # 'IndAudStreamParms' { localDescriptor = LD }
  end | __Stack].

-compile({inline,{yeccpars2_328_,1}}).
-file("megaco_text_parser_v3.yrl", 835).
yeccpars2_328_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   RD = ensure_prop_groups ( __1 ) ,
    # 'IndAudStreamParms' { remoteDescriptor = RD }
  end | __Stack].

-compile({inline,{yeccpars2_333_,1}}).
-file("megaco_text_parser_v3.yrl", 884).
yeccpars2_333_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { prop , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_334_,1}}).
-file("megaco_text_parser_v3.yrl", 885).
yeccpars2_334_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { name , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_337_,1}}).
-file("megaco_text_parser_v3.yrl", 883).
yeccpars2_337_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   bufferToken
  end | __Stack].

-compile({inline,{yeccpars2_338_,1}}).
-file("megaco_text_parser_v3.yrl", 888).
yeccpars2_338_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   serviceStatesToken
  end | __Stack].

-compile({inline,{yeccpars2_341_,1}}).
-file("megaco_text_parser_v3.yrl", 892).
yeccpars2_341_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { serviceStates , { inequal , __3 } }
  end | __Stack].

-compile({inline,{yeccpars2_342_,1}}).
-file("megaco_text_parser_v3.yrl", 1170).
yeccpars2_342_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   inSvc
  end | __Stack].

-compile({inline,{yeccpars2_343_,1}}).
-file("megaco_text_parser_v3.yrl", 1169).
yeccpars2_343_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   outOfSvc
  end | __Stack].

-compile({inline,{yeccpars2_344_,1}}).
-file("megaco_text_parser_v3.yrl", 1168).
yeccpars2_344_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   test
  end | __Stack].

-compile({inline,{yeccpars2_345_,1}}).
-file("megaco_text_parser_v3.yrl", 890).
yeccpars2_345_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { serviceStates , { equal , __3 } }
  end | __Stack].

-compile({inline,{yeccpars2_346_,1}}).
-file("megaco_text_parser_v3.yrl", 876).
yeccpars2_346_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_indAudTerminationStateDescriptor ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_347_,1}}).
-file("megaco_text_parser_v3.yrl", 1112).
yeccpars2_347_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   setelement ( # 'PropertyParm' .name , __2 , __1 )
  end | __Stack].

-compile({inline,{yeccpars2_352_,1}}).
-file("megaco_text_parser_v3.yrl", 847).
yeccpars2_352_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'IndAudStreamDescriptor' { streamID = __3 ,
    streamParms = __5 }
  end | __Stack].

-compile({inline,{yeccpars2_354_,1}}).
-file("megaco_text_parser_v3.yrl", 870).
yeccpars2_354_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { prop , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_355_,1}}).
-file("megaco_text_parser_v3.yrl", 871).
yeccpars2_355_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { name , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_356_,1}}).
-file("megaco_text_parser_v3.yrl", 858).
yeccpars2_356_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_357_,1}}).
-file("megaco_text_parser_v3.yrl", 867).
yeccpars2_357_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   modeToken
  end | __Stack].

-compile({inline,{yeccpars2_358_,1}}).
-file("megaco_text_parser_v3.yrl", 865).
yeccpars2_358_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   reservedGroupToken
  end | __Stack].

-compile({inline,{yeccpars2_359_,1}}).
-file("megaco_text_parser_v3.yrl", 866).
yeccpars2_359_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   reservedValueToken
  end | __Stack].

-compile({inline,{yeccpars2_362_,1}}).
-file("megaco_text_parser_v3.yrl", 869).
yeccpars2_362_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { mode , { inequal , __3 } }
  end | __Stack].

-compile({inline,{yeccpars2_363_,1}}).
-file("megaco_text_parser_v3.yrl", 1108).
yeccpars2_363_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   inactive
  end | __Stack].

-compile({inline,{yeccpars2_364_,1}}).
-file("megaco_text_parser_v3.yrl", 1109).
yeccpars2_364_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   loopBack
  end | __Stack].

-compile({inline,{yeccpars2_365_,1}}).
-file("megaco_text_parser_v3.yrl", 1106).
yeccpars2_365_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   recvOnly
  end | __Stack].

-compile({inline,{yeccpars2_366_,1}}).
-file("megaco_text_parser_v3.yrl", 1105).
yeccpars2_366_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   sendOnly
  end | __Stack].

-compile({inline,{yeccpars2_367_,1}}).
-file("megaco_text_parser_v3.yrl", 1107).
yeccpars2_367_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   sendRecv
  end | __Stack].

-compile({inline,{yeccpars2_368_,1}}).
-file("megaco_text_parser_v3.yrl", 868).
yeccpars2_368_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { mode , { equal , __3 } }
  end | __Stack].

-compile({inline,{yeccpars2_371_,1}}).
-file("megaco_text_parser_v3.yrl", 858).
yeccpars2_371_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_372_,1}}).
-file("megaco_text_parser_v3.yrl", 857).
yeccpars2_372_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_373_,1}}).
-file("megaco_text_parser_v3.yrl", 854).
yeccpars2_373_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_indAudLocalControlDescriptor ( [ __3 | __4 ] )
  end | __Stack].

-compile({inline,{yeccpars2_376_,1}}).
-file("megaco_text_parser_v3.yrl", 831).
yeccpars2_376_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_377_,1}}).
-file("megaco_text_parser_v3.yrl", 830).
yeccpars2_377_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_378_,1}}).
-file("megaco_text_parser_v3.yrl", 820).
yeccpars2_378_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_indAudMediaDescriptor ( [ __3 | __4 ] )
  end | __Stack].

-compile({inline,{yeccpars2_383_,1}}).
-file("megaco_text_parser_v3.yrl", 909).
yeccpars2_383_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'IndAudEventsDescriptor' { pkgdName = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_387_,1}}).
-file("megaco_text_parser_v3.yrl", 912).
yeccpars2_387_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'IndAudEventsDescriptor' { requestID = __3 ,
    pkgdName = __5 }
  end | __Stack].

-compile({inline,{yeccpars2_389_,1}}).
-file("megaco_text_parser_v3.yrl", 902).
yeccpars2_389_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_391_,1}}).
-file("megaco_text_parser_v3.yrl", 895).
yeccpars2_391_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_392_,1}}).
-file("megaco_text_parser_v3.yrl", 898).
yeccpars2_392_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   merge_indAudEventBufferDescriptor ( __1 , __2 )
  end | __Stack].

-compile({inline,{yeccpars2_394_,1}}).
-file("megaco_text_parser_v3.yrl", 1275).
yeccpars2_394_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_NAME ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_396_,1}}).
-file("megaco_text_parser_v3.yrl", 905).
yeccpars2_396_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { streamID , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_397_,1}}).
-file("megaco_text_parser_v3.yrl", 906).
yeccpars2_397_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { eventParameterName , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_398_,1}}).
-file("megaco_text_parser_v3.yrl", 901).
yeccpars2_398_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_399_,1}}).
-file("megaco_text_parser_v3.yrl", 762).
yeccpars2_399_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_auditDescriptor ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_400_,1}}).
-file("megaco_text_parser_v3.yrl", 764).
yeccpars2_400_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_402_,1}}).
-file("megaco_text_parser_v3.yrl", 768).
yeccpars2_402_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_403_,1}}).
-file("megaco_text_parser_v3.yrl", 767).
yeccpars2_403_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_404_,1}}).
-file("megaco_text_parser_v3.yrl", 795).
yeccpars2_404_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_406_,1}}).
-file("megaco_text_parser_v3.yrl", 800).
yeccpars2_406_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_412_,1}}).
-file("megaco_text_parser_v3.yrl", 799).
yeccpars2_412_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_413_,1}}).
-file("megaco_text_parser_v3.yrl", 712).
yeccpars2_413_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_420_,1}}).
-file("megaco_text_parser_v3.yrl", 1440).
yeccpars2_420_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { time_stamp , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_421_,1}}).
-file("megaco_text_parser_v3.yrl", 1442).
yeccpars2_421_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { version , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_422_,1}}).
-file("megaco_text_parser_v3.yrl", 1435).
yeccpars2_422_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { reason , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_423_,1}}).
-file("megaco_text_parser_v3.yrl", 1438).
yeccpars2_423_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { profile , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_424_,1}}).
-file("megaco_text_parser_v3.yrl", 1432).
yeccpars2_424_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_425_,1}}).
-file("megaco_text_parser_v3.yrl", 1441).
yeccpars2_425_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { mgc_id , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_426_,1}}).
-file("megaco_text_parser_v3.yrl", 1434).
yeccpars2_426_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { method , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_427_,1}}).
-file("megaco_text_parser_v3.yrl", 1436).
yeccpars2_427_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { delay , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_428_,1}}).
-file("megaco_text_parser_v3.yrl", 1437).
yeccpars2_428_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { address , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_429_,1}}).
-file("megaco_text_parser_v3.yrl", 1539).
yeccpars2_429_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_extensionParameter ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_431_,1}}).
-file("megaco_text_parser_v3.yrl", 1439).
yeccpars2_431_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { extension , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_432_,1}}).
-file("megaco_text_parser_v3.yrl", 1444).
yeccpars2_432_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { audit_item , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_433_,1}}).
-file("megaco_text_parser_v3.yrl", 1564).
yeccpars2_433_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_434_,1}}).
-file("megaco_text_parser_v3.yrl", 1596).
yeccpars2_434_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_435_,1}}).
-file("megaco_text_parser_v3.yrl", 1597).
yeccpars2_435_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_436_,1}}).
-file("megaco_text_parser_v3.yrl", 1620).
yeccpars2_436_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_437_,1}}).
-file("megaco_text_parser_v3.yrl", 1621).
yeccpars2_437_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_438_,1}}).
-file("megaco_text_parser_v3.yrl", 1638).
yeccpars2_438_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_439_,1}}).
-file("megaco_text_parser_v3.yrl", 1443).
yeccpars2_439_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   incomplete
  end | __Stack].

-compile({inline,{yeccpars2_440_,1}}).
-file("megaco_text_parser_v3.yrl", 1492).
yeccpars2_440_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_timeStamp ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_441_,1}}).
-file("megaco_text_parser_v3.yrl", 1660).
yeccpars2_441_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_443_,1}}).
-file("megaco_text_parser_v3.yrl", 1461).
yeccpars2_443_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_version ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_444_,1}}).
-file("megaco_text_parser_v3.yrl", 484).
yeccpars2_444_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_445_,1}}).
-file("megaco_text_parser_v3.yrl", 1455).
yeccpars2_445_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { portNumber , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_446_,1}}).
-file("megaco_text_parser_v3.yrl", 1453).
yeccpars2_446_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_448_,1}}).
-file("megaco_text_parser_v3.yrl", 1449).
yeccpars2_448_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_450_,1}}).
-file("megaco_text_parser_v3.yrl", 1459).
yeccpars2_450_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_profile ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_451_,1}}).
-file("megaco_text_parser_v3.yrl", 484).
yeccpars2_451_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_452_,1}}).
-file("megaco_text_parser_v3.yrl", 1457).
yeccpars2_452_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_454_,1}}).
-file("megaco_text_parser_v3.yrl", 1447).
yeccpars2_454_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_serviceChangeMethod ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_456_,1}}).
-file("megaco_text_parser_v3.yrl", 1451).
yeccpars2_456_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_uint32 ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_457_,1}}).
-file("megaco_text_parser_v3.yrl", 1464).
yeccpars2_457_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   setelement ( # 'PropertyParm' .name , __2 , __1 )
  end | __Stack].

-compile({inline,{yeccpars2_460_,1}}).
-file("megaco_text_parser_v3.yrl", 1432).
yeccpars2_460_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_461_,1}}).
-file("megaco_text_parser_v3.yrl", 1431).
yeccpars2_461_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_462_,1}}).
-file("megaco_text_parser_v3.yrl", 1428).
yeccpars2_462_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_ServiceChangeParm ( [ __3 | __4 ] )
  end | __Stack].

-compile({inline,{yeccpars2_463_,1}}).
-file("megaco_text_parser_v3.yrl", 972).
yeccpars2_463_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   make_commandRequest ( { serviceChangeReq , __1 } ,
    # 'ServiceChangeRequest' { terminationID = __3 ,
    serviceChangeParms = __5 } )
  end | __Stack].

-compile({inline,{yeccpars2_465_,1}}).
-file("megaco_text_parser_v3.yrl", 1537).
yeccpars2_465_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   ensure_uint16 ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_469_,1}}).
-file("megaco_text_parser_v3.yrl", 959).
yeccpars2_469_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'NotifyRequest' { observedEventsDescriptor = __1 }
  end | __Stack].

-compile({inline,{yeccpars2_471_,1}}).
-file("megaco_text_parser_v3.yrl", 961).
yeccpars2_471_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'NotifyRequest' { errorDescriptor = __1 }
  end | __Stack].

-compile({inline,{yeccpars2_475_,1}}).
-file("megaco_text_parser_v3.yrl", 484).
yeccpars2_475_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_476_,1}}).
-file("megaco_text_parser_v3.yrl", 484).
yeccpars2_476_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_478_,1}}).
-file("megaco_text_parser_v3.yrl", 1378).
yeccpars2_478_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_480_,1}}).
-file("megaco_text_parser_v3.yrl", 484).
yeccpars2_480_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_481_,1}}).
-file("megaco_text_parser_v3.yrl", 1378).
yeccpars2_481_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_482_,1}}).
-file("megaco_text_parser_v3.yrl", 1377).
yeccpars2_482_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_483_,1}}).
-file("megaco_text_parser_v3.yrl", 1374).
yeccpars2_483_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'ObservedEventsDescriptor' { requestId = __3 ,
    observedEventLst = [ __5 | __6 ] }
  end | __Stack].

-compile({inline,{yeccpars2_484_,1}}).
-file("megaco_text_parser_v3.yrl", 1390).
yeccpars2_484_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_485_,1}}).
-file("megaco_text_parser_v3.yrl", 1385).
yeccpars2_485_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_observed_event ( __3 , __2 , asn1_NOVALUE )
  end | __Stack].

-compile({inline,{yeccpars2_487_,1}}).
-file("megaco_text_parser_v3.yrl", 1393).
yeccpars2_487_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_490_,1}}).
-file("megaco_text_parser_v3.yrl", 1273).
yeccpars2_490_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   select_stream_or_other ( __1 , __2 )
  end | __Stack].

-compile({inline,{yeccpars2_493_,1}}).
-file("megaco_text_parser_v3.yrl", 1393).
yeccpars2_493_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_494_,1}}).
-file("megaco_text_parser_v3.yrl", 1392).
yeccpars2_494_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_495_,1}}).
-file("megaco_text_parser_v3.yrl", 1389).
yeccpars2_495_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_497_,1}}).
-file("megaco_text_parser_v3.yrl", 484).
yeccpars2_497_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_499_,1}}).
-file("megaco_text_parser_v3.yrl", 1390).
yeccpars2_499_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_500_,1}}).
-file("megaco_text_parser_v3.yrl", 1383).
yeccpars2_500_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_observed_event ( __6 , __5 , __1 )
  end | __Stack].

-compile({inline,{yeccpars2_501_,1}}).
-file("megaco_text_parser_v3.yrl", 954).
yeccpars2_501_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   NR = setelement ( # 'NotifyRequest' .terminationID ,
    __5 , __3 ) ,
    make_commandRequest ( { notifyReq , __1 } , NR )
  end | __Stack].

-compile({inline,{yeccpars2_503_,1}}).
-file("megaco_text_parser_v3.yrl", 1532).
yeccpars2_503_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_504_,1}}).
-file("megaco_text_parser_v3.yrl", 1102).
yeccpars2_504_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   false
  end | __Stack].

-compile({inline,{yeccpars2_505_,1}}).
-file("megaco_text_parser_v3.yrl", 1101).
yeccpars2_505_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   true
  end | __Stack].

-compile({inline,{yeccpars2_507_,1}}).
-file("megaco_text_parser_v3.yrl", 603).
yeccpars2_507_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { select_prio , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_508_,1}}).
-file("megaco_text_parser_v3.yrl", 599).
yeccpars2_508_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { prop , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_510_,1}}).
-file("megaco_text_parser_v3.yrl", 605).
yeccpars2_510_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { select_ieps , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_511_,1}}).
-file("megaco_text_parser_v3.yrl", 604).
yeccpars2_511_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { select_emergency , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_513_,1}}).
-file("megaco_text_parser_v3.yrl", 592).
yeccpars2_513_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_515_,1}}).
-file("megaco_text_parser_v3.yrl", 606).
yeccpars2_515_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { select_logic , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_516_,1}}).
-file("megaco_text_parser_v3.yrl", 609).
yeccpars2_516_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { andAUDITSelect , 'NULL' }
  end | __Stack].

-compile({inline,{yeccpars2_518_,1}}).
-file("megaco_text_parser_v3.yrl", 596).
yeccpars2_518_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   emergencyAudit
  end | __Stack].

-compile({inline,{yeccpars2_520_,1}}).
-file("megaco_text_parser_v3.yrl", 598).
yeccpars2_520_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   iepsCallind
  end | __Stack].

-compile({inline,{yeccpars2_521_,1}}).
-file("megaco_text_parser_v3.yrl", 610).
yeccpars2_521_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { orAUDITSelect , 'NULL' }
  end | __Stack].

-compile({inline,{yeccpars2_522_,1}}).
-file("megaco_text_parser_v3.yrl", 597).
yeccpars2_522_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   priorityAudit
  end | __Stack].

-compile({inline,{yeccpars2_523_,1}}).
-file("megaco_text_parser_v3.yrl", 595).
yeccpars2_523_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   topologyAudit
  end | __Stack].

-compile({inline,{yeccpars2_525_,1}}).
-file("megaco_text_parser_v3.yrl", 1535).
yeccpars2_525_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   false
  end | __Stack].

-compile({inline,{yeccpars2_526_,1}}).
-file("megaco_text_parser_v3.yrl", 1534).
yeccpars2_526_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   true
  end | __Stack].

-compile({inline,{yeccpars2_529_,1}}).
-file("megaco_text_parser_v3.yrl", 1414).
yeccpars2_529_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_530_,1}}).
-file("megaco_text_parser_v3.yrl", 599).
yeccpars2_530_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { prop , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_532_,1}}).
-file("megaco_text_parser_v3.yrl", 592).
yeccpars2_532_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_536_,1}}).
-file("megaco_text_parser_v3.yrl", 574).
yeccpars2_536_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_539_,1}}).
-file("megaco_text_parser_v3.yrl", 574).
yeccpars2_539_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_540_,1}}).
-file("megaco_text_parser_v3.yrl", 573).
yeccpars2_540_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_541_,1}}).
-file("megaco_text_parser_v3.yrl", 571).
yeccpars2_541_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __4 | __5 ]
  end | __Stack].

-compile({inline,{yeccpars2_544_,1}}).
-file("megaco_text_parser_v3.yrl", 592).
yeccpars2_544_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_545_,1}}).
-file("megaco_text_parser_v3.yrl", 591).
yeccpars2_545_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_546_,1}}).
-file("megaco_text_parser_v3.yrl", 588).
yeccpars2_546_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_547_,1}}).
-file("megaco_text_parser_v3.yrl", 568).
yeccpars2_547_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { contextList , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_548_,1}}).
-file("megaco_text_parser_v3.yrl", 1412).
yeccpars2_548_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_550_,1}}).
-file("megaco_text_parser_v3.yrl", 1414).
yeccpars2_550_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_552_,1}}).
-file("megaco_text_parser_v3.yrl", 1413).
yeccpars2_552_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_553_,1}}).
-file("megaco_text_parser_v3.yrl", 566).
yeccpars2_553_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { contextProp , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_555_,1}}).
-file("megaco_text_parser_v3.yrl", 582).
yeccpars2_555_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_context_attr_audit_request (
    # 'ContextAttrAuditRequest' { } , [ __3 | __4 ] )
  end | __Stack].

-compile({inline,{yeccpars2_556_,1}}).
-file("megaco_text_parser_v3.yrl", 578).
yeccpars2_556_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_context_attr_audit_request (
    # 'ContextAttrAuditRequest' { } , __3 )
  end | __Stack].

-compile({inline,{yeccpars2_559_,1}}).
-file("megaco_text_parser_v3.yrl", 713).
yeccpars2_559_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_560_,1}}).
-file("megaco_text_parser_v3.yrl", 716).
yeccpars2_560_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   make_commandRequest ( { auditValueRequest , __1 } ,
    make_auditRequest ( __3 , __4 ) )
  end | __Stack].

-compile({inline,{yeccpars2_562_,1}}).
-file("megaco_text_parser_v3.yrl", 713).
yeccpars2_562_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_563_,1}}).
-file("megaco_text_parser_v3.yrl", 719).
yeccpars2_563_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   make_commandRequest ( { auditCapRequest , __1 } ,
    make_auditRequest ( __3 , __4 ) )
  end | __Stack].

-compile({inline,{yeccpars2_564_,1}}).
-file("megaco_text_parser_v3.yrl", 544).
yeccpars2_564_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_action_request ( __3 , __5 )
  end | __Stack].

-compile({inline,{yeccpars2_565_,1}}).
-file("megaco_text_parser_v3.yrl", 546).
yeccpars2_565_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_567_,1}}).
-file("megaco_text_parser_v3.yrl", 550).
yeccpars2_567_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_568_,1}}).
-file("megaco_text_parser_v3.yrl", 549).
yeccpars2_568_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_570_,1}}).
-file("megaco_text_parser_v3.yrl", 678).
yeccpars2_570_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_571_,1}}).
-file("megaco_text_parser_v3.yrl", 668).
yeccpars2_571_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   Descs = merge_AmmRequest_descriptors ( __4 , [ ] ) ,
    make_commandRequest ( __1 ,
    # 'AmmRequest' { terminationID = __3 ,
    descriptors = Descs } )
  end | __Stack].

-compile({inline,{yeccpars2_573_,1}}).
-file("megaco_text_parser_v3.yrl", 692).
yeccpars2_573_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { statisticsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_574_,1}}).
-file("megaco_text_parser_v3.yrl", 689).
yeccpars2_574_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { signalsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_575_,1}}).
-file("megaco_text_parser_v3.yrl", 686).
yeccpars2_575_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { muxDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_576_,1}}).
-file("megaco_text_parser_v3.yrl", 685).
yeccpars2_576_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { modemDescriptor , deprecated }
  end | __Stack].

-compile({inline,{yeccpars2_577_,1}}).
-file("megaco_text_parser_v3.yrl", 684).
yeccpars2_577_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { mediaDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_578_,1}}).
-file("megaco_text_parser_v3.yrl", 687).
yeccpars2_578_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { eventsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_579_,1}}).
-file("megaco_text_parser_v3.yrl", 688).
yeccpars2_579_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { eventBufferDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_580_,1}}).
-file("megaco_text_parser_v3.yrl", 690).
yeccpars2_580_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { digitMapDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_581_,1}}).
-file("megaco_text_parser_v3.yrl", 691).
yeccpars2_581_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { auditDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_582_,1}}).
-file("megaco_text_parser_v3.yrl", 681).
yeccpars2_582_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_583_,1}}).
-file("megaco_text_parser_v3.yrl", 1420).
yeccpars2_583_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_DMD ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_584_,1}}).
-file("megaco_text_parser_v3.yrl", 1151).
yeccpars2_584_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_585_,1}}).
-file("megaco_text_parser_v3.yrl", 1188).
yeccpars2_585_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'EventsDescriptor' { requestID = asn1_NOVALUE ,
    eventList = [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_589_,1}}).
-file("megaco_text_parser_v3.yrl", 1284).
yeccpars2_589_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_592_,1}}).
-file("megaco_text_parser_v3.yrl", 1500).
yeccpars2_592_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_593_,1}}).
-file("megaco_text_parser_v3.yrl", 1504).
yeccpars2_593_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'StatisticsParameter' { statName = __1 ,
    statValue = asn1_NOVALUE }
  end | __Stack].

-compile({inline,{yeccpars2_595_,1}}).
-file("megaco_text_parser_v3.yrl", 1507).
yeccpars2_595_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'StatisticsParameter' { statName = __1 ,
    statValue = [ __3 ] }
  end | __Stack].

-compile({inline,{yeccpars2_597_,1}}).
-file("megaco_text_parser_v3.yrl", 1148).
yeccpars2_597_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_599_,1}}).
-file("megaco_text_parser_v3.yrl", 1510).
yeccpars2_599_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'StatisticsParameter' { statName = __1 ,
    statValue = [ __4 | __5 ] }
  end | __Stack].

-compile({inline,{yeccpars2_602_,1}}).
-file("megaco_text_parser_v3.yrl", 1500).
yeccpars2_602_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_603_,1}}).
-file("megaco_text_parser_v3.yrl", 1499).
yeccpars2_603_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_604_,1}}).
-file("megaco_text_parser_v3.yrl", 1497).
yeccpars2_604_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_606_,1}}).
-file("megaco_text_parser_v3.yrl", 1290).
yeccpars2_606_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { signal , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_607_,1}}).
-file("megaco_text_parser_v3.yrl", 1287).
yeccpars2_607_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_608_,1}}).
-file("megaco_text_parser_v3.yrl", 1289).
yeccpars2_608_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { seqSigList , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_609_,1}}).
-file("megaco_text_parser_v3.yrl", 1639).
yeccpars2_609_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_613_,1}}).
-file("megaco_text_parser_v3.yrl", 1362).
yeccpars2_613_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_616_,1}}).
-file("megaco_text_parser_v3.yrl", 1362).
yeccpars2_616_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_617_,1}}).
-file("megaco_text_parser_v3.yrl", 1361).
yeccpars2_617_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_618_,1}}).
-file("megaco_text_parser_v3.yrl", 1357).
yeccpars2_618_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'SeqSigList' { id = ensure_uint16 ( __3 ) ,
    signalList = [ __5 | __6 ] }
  end | __Stack].

-compile({inline,{yeccpars2_621_,1}}).
-file("megaco_text_parser_v3.yrl", 1287).
yeccpars2_621_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_622_,1}}).
-file("megaco_text_parser_v3.yrl", 1286).
yeccpars2_622_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_623_,1}}).
-file("megaco_text_parser_v3.yrl", 1283).
yeccpars2_623_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_625_,1}}).
-file("megaco_text_parser_v3.yrl", 1181).
yeccpars2_625_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_muxType ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_627_,1}}).
-file("megaco_text_parser_v3.yrl", 1178).
yeccpars2_627_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'MuxDescriptor' { muxType = __3 ,
    termList = __4 }
  end | __Stack].

-compile({inline,{yeccpars2_629_,1}}).
-file("megaco_text_parser_v3.yrl", 1041).
yeccpars2_629_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_631_,1}}).
-file("megaco_text_parser_v3.yrl", 1037).
yeccpars2_631_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_634_,1}}).
-file("megaco_text_parser_v3.yrl", 0).
yeccpars2_634_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_635_,1}}).
-file("megaco_text_parser_v3.yrl", 0).
yeccpars2_635_(__Stack0) ->
 [begin
   '$undefined'
  end | __Stack0].

-compile({inline,{yeccpars2_638_,1}}).
-file("megaco_text_parser_v3.yrl", 0).
yeccpars2_638_(__Stack0) ->
 [begin
   '$undefined'
  end | __Stack0].

-compile({inline,{yeccpars2_639_,1}}).
-file("megaco_text_parser_v3.yrl", 0).
yeccpars2_639_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_640_,1}}).
-file("megaco_text_parser_v3.yrl", 1410).
yeccpars2_640_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_641_,1}}).
-file("megaco_text_parser_v3.yrl", 0).
yeccpars2_641_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_643_,1}}).
-file("megaco_text_parser_v3.yrl", 1414).
yeccpars2_643_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_645_,1}}).
-file("megaco_text_parser_v3.yrl", 1409).
yeccpars2_645_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_646_,1}}).
-file("megaco_text_parser_v3.yrl", 1410).
yeccpars2_646_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_647_,1}}).
-file("megaco_text_parser_v3.yrl", 0).
yeccpars2_647_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_649_,1}}).
-file("megaco_text_parser_v3.yrl", 1062).
yeccpars2_649_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { termState , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_650_,1}}).
-file("megaco_text_parser_v3.yrl", 1058).
yeccpars2_650_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { streamParm , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_651_,1}}).
-file("megaco_text_parser_v3.yrl", 1060).
yeccpars2_651_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { streamDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_652_,1}}).
-file("megaco_text_parser_v3.yrl", 1071).
yeccpars2_652_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { statistics , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_653_,1}}).
-file("megaco_text_parser_v3.yrl", 1052).
yeccpars2_653_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_654_,1}}).
-file("megaco_text_parser_v3.yrl", 1070).
yeccpars2_654_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { control , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_656_,1}}).
-file("megaco_text_parser_v3.yrl", 1067).
yeccpars2_656_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { local , # 'LocalRemoteDescriptor' { propGrps = ensure_prop_groups ( __1 ) } }
  end | __Stack].

-compile({inline,{yeccpars2_657_,1}}).
-file("megaco_text_parser_v3.yrl", 1069).
yeccpars2_657_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { remote , # 'LocalRemoteDescriptor' { propGrps = ensure_prop_groups ( __1 ) } }
  end | __Stack].

-compile({inline,{yeccpars2_661_,1}}).
-file("megaco_text_parser_v3.yrl", 1093).
yeccpars2_661_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_662_,1}}).
-file("megaco_text_parser_v3.yrl", 1162).
yeccpars2_662_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { serviceState , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_663_,1}}).
-file("megaco_text_parser_v3.yrl", 1164).
yeccpars2_663_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { propertyParm , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_664_,1}}).
-file("megaco_text_parser_v3.yrl", 1163).
yeccpars2_664_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { eventBufferControl , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_668_,1}}).
-file("megaco_text_parser_v3.yrl", 1166).
yeccpars2_668_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_670_,1}}).
-file("megaco_text_parser_v3.yrl", 1172).
yeccpars2_670_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __3
  end | __Stack].

-compile({inline,{yeccpars2_671_,1}}).
-file("megaco_text_parser_v3.yrl", 1175).
yeccpars2_671_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   lockStep
  end | __Stack].

-compile({inline,{yeccpars2_672_,1}}).
-file("megaco_text_parser_v3.yrl", 1174).
yeccpars2_672_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   off
  end | __Stack].

-compile({inline,{yeccpars2_675_,1}}).
-file("megaco_text_parser_v3.yrl", 1093).
yeccpars2_675_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_676_,1}}).
-file("megaco_text_parser_v3.yrl", 1092).
yeccpars2_676_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_677_,1}}).
-file("megaco_text_parser_v3.yrl", 1090).
yeccpars2_677_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_terminationStateDescriptor ( [ __3 | __4 ] )
  end | __Stack].

-compile({inline,{yeccpars2_681_,1}}).
-file("megaco_text_parser_v3.yrl", 1079).
yeccpars2_681_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_684_,1}}).
-file("megaco_text_parser_v3.yrl", 1079).
yeccpars2_684_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_685_,1}}).
-file("megaco_text_parser_v3.yrl", 1078).
yeccpars2_685_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_686_,1}}).
-file("megaco_text_parser_v3.yrl", 1075).
yeccpars2_686_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'StreamDescriptor' { streamID = __3 ,
    streamParms = merge_streamParms ( [ __5 | __6 ] ) }
  end | __Stack].

-compile({inline,{yeccpars2_688_,1}}).
-file("megaco_text_parser_v3.yrl", 1099).
yeccpars2_688_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { prop , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_689_,1}}).
-file("megaco_text_parser_v3.yrl", 1085).
yeccpars2_689_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_694_,1}}).
-file("megaco_text_parser_v3.yrl", 1097).
yeccpars2_694_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { value , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_696_,1}}).
-file("megaco_text_parser_v3.yrl", 1096).
yeccpars2_696_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { group , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_698_,1}}).
-file("megaco_text_parser_v3.yrl", 1098).
yeccpars2_698_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { mode , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_701_,1}}).
-file("megaco_text_parser_v3.yrl", 1085).
yeccpars2_701_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_702_,1}}).
-file("megaco_text_parser_v3.yrl", 1084).
yeccpars2_702_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_703_,1}}).
-file("megaco_text_parser_v3.yrl", 1082).
yeccpars2_703_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_706_,1}}).
-file("megaco_text_parser_v3.yrl", 1052).
yeccpars2_706_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_707_,1}}).
-file("megaco_text_parser_v3.yrl", 1051).
yeccpars2_707_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_708_,1}}).
-file("megaco_text_parser_v3.yrl", 1049).
yeccpars2_708_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_mediaDescriptor ( [ __3 | __4 ] )
  end | __Stack].

-compile({inline,{yeccpars2_712_,1}}).
-file("megaco_text_parser_v3.yrl", 1196).
yeccpars2_712_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_713_,1}}).
-file("megaco_text_parser_v3.yrl", 1203).
yeccpars2_713_(__Stack0) ->
 [begin
   # 'RequestedEvent' { evParList = [ ] }
  end | __Stack0].

-compile({inline,{yeccpars2_714_,1}}).
-file("megaco_text_parser_v3.yrl", 1199).
yeccpars2_714_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   setelement ( # 'RequestedEvent' .pkgdName , __2 , __1 )
  end | __Stack].

-compile({inline,{yeccpars2_716_,1}}).
-file("megaco_text_parser_v3.yrl", 1215).
yeccpars2_716_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { notifyRegulated , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_717_,1}}).
-file("megaco_text_parser_v3.yrl", 1227).
yeccpars2_717_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { notifyBehaviour , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_719_,1}}).
-file("megaco_text_parser_v3.yrl", 1219).
yeccpars2_719_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_723_,1}}).
-file("megaco_text_parser_v3.yrl", 1279).
yeccpars2_723_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_eventDM ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_724_,1}}).
-file("megaco_text_parser_v3.yrl", 1566).
yeccpars2_724_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_725_COMMA,1}}).
-file("megaco_text_parser_v3.yrl", 1222).
yeccpars2_725_COMMA(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   keepActive
  end | __Stack].

-compile({inline,{yeccpars2_725_RBRKT,1}}).
-file("megaco_text_parser_v3.yrl", 1222).
yeccpars2_725_RBRKT(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   keepActive
  end | __Stack].

-compile({inline,{yeccpars2_725_,1}}).
-file("megaco_text_parser_v3.yrl", 1588).
yeccpars2_725_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_726_,1}}).
-file("megaco_text_parser_v3.yrl", 1214).
yeccpars2_726_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { neverNotify , 'NULL' }
  end | __Stack].

-compile({inline,{yeccpars2_727_,1}}).
-file("megaco_text_parser_v3.yrl", 1213).
yeccpars2_727_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { notifyImmediate , 'NULL' }
  end | __Stack].

-compile({inline,{yeccpars2_728_,1}}).
-file("megaco_text_parser_v3.yrl", 1207).
yeccpars2_728_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'RegulatedEmbeddedDescriptor' { }
  end | __Stack].

-compile({inline,{yeccpars2_729_,1}}).
-file("megaco_text_parser_v3.yrl", 1228).
yeccpars2_729_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   resetEventsDescriptor
  end | __Stack].

-compile({inline,{yeccpars2_737_,1}}).
-file("megaco_text_parser_v3.yrl", 1240).
yeccpars2_737_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'SecondEventsDescriptor' { requestID = asn1_NOVALUE ,
    eventList = [ ] }
  end | __Stack].

-compile({inline,{yeccpars2_741_,1}}).
-file("megaco_text_parser_v3.yrl", 1248).
yeccpars2_741_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_742_,1}}).
-file("megaco_text_parser_v3.yrl", 1256).
yeccpars2_742_(__Stack0) ->
 [begin
   # 'SecondRequestedEvent' { evParList = [ ] }
  end | __Stack0].

-compile({inline,{yeccpars2_743_,1}}).
-file("megaco_text_parser_v3.yrl", 1252).
yeccpars2_743_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   setelement ( # 'SecondRequestedEvent' .pkgdName , __2 , __1 )
  end | __Stack].

-compile({inline,{yeccpars2_745_,1}}).
-file("megaco_text_parser_v3.yrl", 1259).
yeccpars2_745_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_746_,1}}).
-file("megaco_text_parser_v3.yrl", 1266).
yeccpars2_746_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { notifyBehaviour , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_750_,1}}).
-file("megaco_text_parser_v3.yrl", 1566).
yeccpars2_750_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_751_COMMA,1}}).
-file("megaco_text_parser_v3.yrl", 1262).
yeccpars2_751_COMMA(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   keepActive
  end | __Stack].

-compile({inline,{yeccpars2_751_RBRKT,1}}).
-file("megaco_text_parser_v3.yrl", 1262).
yeccpars2_751_RBRKT(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   keepActive
  end | __Stack].

-compile({inline,{yeccpars2_751_,1}}).
-file("megaco_text_parser_v3.yrl", 1588).
yeccpars2_751_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_752_,1}}).
-file("megaco_text_parser_v3.yrl", 1267).
yeccpars2_752_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   resetEventsDescriptor
  end | __Stack].

-compile({inline,{yeccpars2_755_,1}}).
-file("megaco_text_parser_v3.yrl", 1270).
yeccpars2_755_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { second_embed , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_758_,1}}).
-file("megaco_text_parser_v3.yrl", 1259).
yeccpars2_758_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_759_,1}}).
-file("megaco_text_parser_v3.yrl", 1258).
yeccpars2_759_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_760_,1}}).
-file("megaco_text_parser_v3.yrl", 1255).
yeccpars2_760_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_secondEventParameters ( [ __2 | __3 ] )
  end | __Stack].

-compile({inline,{yeccpars2_763_,1}}).
-file("megaco_text_parser_v3.yrl", 1248).
yeccpars2_763_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_764_,1}}).
-file("megaco_text_parser_v3.yrl", 1247).
yeccpars2_764_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_765_,1}}).
-file("megaco_text_parser_v3.yrl", 1244).
yeccpars2_765_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'SecondEventsDescriptor' { requestID = __3 ,
    eventList = [ __5 | __6 ] }
  end | __Stack].

-compile({inline,{yeccpars2_766_,1}}).
-file("megaco_text_parser_v3.yrl", 1237).
yeccpars2_766_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { embed , asn1_NOVALUE , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_768_,1}}).
-file("megaco_text_parser_v3.yrl", 1234).
yeccpars2_768_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { embed , __3 , asn1_NOVALUE }
  end | __Stack].

-compile({inline,{yeccpars2_770_,1}}).
-file("megaco_text_parser_v3.yrl", 1232).
yeccpars2_770_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { embed , __3 , __5 }
  end | __Stack].

-compile({inline,{yeccpars2_771_,1}}).
-file("megaco_text_parser_v3.yrl", 1211).
yeccpars2_771_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   make_RegulatedEmbeddedDescriptor ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_772_,1}}).
-file("megaco_text_parser_v3.yrl", 1209).
yeccpars2_772_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   make_RegulatedEmbeddedDescriptor ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_775_,1}}).
-file("megaco_text_parser_v3.yrl", 1219).
yeccpars2_775_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_776_,1}}).
-file("megaco_text_parser_v3.yrl", 1218).
yeccpars2_776_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_777_,1}}).
-file("megaco_text_parser_v3.yrl", 1202).
yeccpars2_777_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_eventParameters ( [ __2 | __3 ] )
  end | __Stack].

-compile({inline,{yeccpars2_780_,1}}).
-file("megaco_text_parser_v3.yrl", 1196).
yeccpars2_780_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_781_,1}}).
-file("megaco_text_parser_v3.yrl", 1195).
yeccpars2_781_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_782_,1}}).
-file("megaco_text_parser_v3.yrl", 1192).
yeccpars2_782_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'EventsDescriptor' { requestID = __3 ,
    eventList = [ __5 | __6 ] }
  end | __Stack].

-compile({inline,{yeccpars2_783_,1}}).
-file("megaco_text_parser_v3.yrl", 484).
yeccpars2_783_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_784_,1}}).
-file("megaco_text_parser_v3.yrl", 1159).
yeccpars2_784_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   merge_eventSpec ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_785_,1}}).
-file("megaco_text_parser_v3.yrl", 1157).
yeccpars2_785_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_787_,1}}).
-file("megaco_text_parser_v3.yrl", 484).
yeccpars2_787_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_788_,1}}).
-file("megaco_text_parser_v3.yrl", 1157).
yeccpars2_788_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_789_,1}}).
-file("megaco_text_parser_v3.yrl", 1156).
yeccpars2_789_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_790_,1}}).
-file("megaco_text_parser_v3.yrl", 1154).
yeccpars2_790_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_793_,1}}).
-file("megaco_text_parser_v3.yrl", 681).
yeccpars2_793_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_794_,1}}).
-file("megaco_text_parser_v3.yrl", 680).
yeccpars2_794_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_795_,1}}).
-file("megaco_text_parser_v3.yrl", 677).
yeccpars2_795_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_798_,1}}).
-file("megaco_text_parser_v3.yrl", 533).
yeccpars2_798_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_799_,1}}).
-file("megaco_text_parser_v3.yrl", 532).
yeccpars2_799_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_800_,1}}).
-file("megaco_text_parser_v3.yrl", 521).
yeccpars2_800_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'TransactionRequest' { transactionId = asn1_NOVALUE ,
    actions = [ __3 | __4 ] }
  end | __Stack].

-compile({inline,{yeccpars2_802_,1}}).
-file("megaco_text_parser_v3.yrl", 999).
yeccpars2_802_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_uint32 ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_804_,1}}).
-file("megaco_text_parser_v3.yrl", 533).
yeccpars2_804_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_806_,1}}).
-file("megaco_text_parser_v3.yrl", 525).
yeccpars2_806_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'TransactionRequest' { transactionId = asn1_NOVALUE ,
    actions = [ __4 | __5 ] }
  end | __Stack].

-compile({inline,{yeccpars2_808_,1}}).
-file("megaco_text_parser_v3.yrl", 533).
yeccpars2_808_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_810_,1}}).
-file("megaco_text_parser_v3.yrl", 529).
yeccpars2_810_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'TransactionRequest' { transactionId = ensure_transactionID ( __3 ) ,
    actions = [ __5 | __6 ] }
  end | __Stack].

-compile({inline,{yeccpars2_812_,1}}).
-file("megaco_text_parser_v3.yrl", 512).
yeccpars2_812_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_813_,1}}).
-file("megaco_text_parser_v3.yrl", 514).
yeccpars2_813_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_transactionAck ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_816_,1}}).
-file("megaco_text_parser_v3.yrl", 512).
yeccpars2_816_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_817_,1}}).
-file("megaco_text_parser_v3.yrl", 511).
yeccpars2_817_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_818_,1}}).
-file("megaco_text_parser_v3.yrl", 508).
yeccpars2_818_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_821_,1}}).
-file("megaco_text_parser_v3.yrl", 1001).
yeccpars2_821_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_transactionID_and_segment_info ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_822_,1}}).
-file("megaco_text_parser_v3.yrl", 631).
yeccpars2_822_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_825_,1}}).
-file("megaco_text_parser_v3.yrl", 630).
yeccpars2_825_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   'NULL'
  end | __Stack].

-compile({inline,{yeccpars2_827_,1}}).
-file("megaco_text_parser_v3.yrl", 633).
yeccpars2_827_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { transactionError , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_828_,1}}).
-file("megaco_text_parser_v3.yrl", 637).
yeccpars2_828_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_831_,1}}).
-file("megaco_text_parser_v3.yrl", 643).
yeccpars2_831_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'ActionReply' { contextId = __3 }
  end | __Stack].

-compile({inline,{yeccpars2_833_,1}}).
-file("megaco_text_parser_v3.yrl", 660).
yeccpars2_833_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { command , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_834_,1}}).
-file("megaco_text_parser_v3.yrl", 663).
yeccpars2_834_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { command , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_835_,1}}).
-file("megaco_text_parser_v3.yrl", 646).
yeccpars2_835_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   # 'ActionReply' { errorDescriptor = __1 }
  end | __Stack].

-compile({inline,{yeccpars2_836_,1}}).
-file("megaco_text_parser_v3.yrl", 664).
yeccpars2_836_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { context , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_837_,1}}).
-file("megaco_text_parser_v3.yrl", 658).
yeccpars2_837_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_838_,1}}).
-file("megaco_text_parser_v3.yrl", 661).
yeccpars2_838_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { command , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_840_,1}}).
-file("megaco_text_parser_v3.yrl", 662).
yeccpars2_840_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { command , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_842_,1}}).
-file("megaco_text_parser_v3.yrl", 698).
yeccpars2_842_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   addReply
  end | __Stack].

-compile({inline,{yeccpars2_845_,1}}).
-file("megaco_text_parser_v3.yrl", 700).
yeccpars2_845_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   modReply
  end | __Stack].

-compile({inline,{yeccpars2_846_,1}}).
-file("megaco_text_parser_v3.yrl", 699).
yeccpars2_846_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   moveReply
  end | __Stack].

-compile({inline,{yeccpars2_849_,1}}).
-file("megaco_text_parser_v3.yrl", 701).
yeccpars2_849_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   subtractReply
  end | __Stack].

-compile({inline,{yeccpars2_851_,1}}).
-file("megaco_text_parser_v3.yrl", 987).
yeccpars2_851_(__Stack0) ->
 [begin
   { serviceChangeResParms , # 'ServiceChangeResParm' { } }
  end | __Stack0].

-compile({inline,{yeccpars2_852_,1}}).
-file("megaco_text_parser_v3.yrl", 978).
yeccpars2_852_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { serviceChangeReply ,
    # 'ServiceChangeReply' { terminationID = __3 ,
    serviceChangeResult = __4 } }
  end | __Stack].

-compile({inline,{yeccpars2_858_,1}}).
-file("megaco_text_parser_v3.yrl", 1481).
yeccpars2_858_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { time_stamp , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_859_,1}}).
-file("megaco_text_parser_v3.yrl", 1480).
yeccpars2_859_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { version , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_860_,1}}).
-file("megaco_text_parser_v3.yrl", 1479).
yeccpars2_860_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { profile , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_861_,1}}).
-file("megaco_text_parser_v3.yrl", 1478).
yeccpars2_861_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { mgc_id , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_862_,1}}).
-file("megaco_text_parser_v3.yrl", 1477).
yeccpars2_862_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { address , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_863_,1}}).
-file("megaco_text_parser_v3.yrl", 1475).
yeccpars2_863_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_870_,1}}).
-file("megaco_text_parser_v3.yrl", 1475).
yeccpars2_870_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_871_,1}}).
-file("megaco_text_parser_v3.yrl", 1474).
yeccpars2_871_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_872_,1}}).
-file("megaco_text_parser_v3.yrl", 1471).
yeccpars2_872_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_ServiceChangeResParm ( [ __3 | __4 ] )
  end | __Stack].

-compile({inline,{yeccpars2_873_,1}}).
-file("megaco_text_parser_v3.yrl", 983).
yeccpars2_873_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { errorDescriptor , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_874_,1}}).
-file("megaco_text_parser_v3.yrl", 985).
yeccpars2_874_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { serviceChangeResParms , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_876_,1}}).
-file("megaco_text_parser_v3.yrl", 968).
yeccpars2_876_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_877_,1}}).
-file("megaco_text_parser_v3.yrl", 964).
yeccpars2_877_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { notifyReply , # 'NotifyReply' { terminationID = __3 ,
    errorDescriptor = __4 } }
  end | __Stack].

-compile({inline,{yeccpars2_880_,1}}).
-file("megaco_text_parser_v3.yrl", 967).
yeccpars2_880_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_882_,1}}).
-file("megaco_text_parser_v3.yrl", 737).
yeccpars2_882_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   merge_auditOther ( __1 , [ ] )
  end | __Stack].

-compile({inline,{yeccpars2_883_,1}}).
-file("megaco_text_parser_v3.yrl", 727).
yeccpars2_883_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { auditValueReply , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_884_,1}}).
-file("megaco_text_parser_v3.yrl", 1554).
yeccpars2_884_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_885_,1}}).
-file("megaco_text_parser_v3.yrl", 732).
yeccpars2_885_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { contextAuditResult , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_886_,1}}).
-file("megaco_text_parser_v3.yrl", 723).
yeccpars2_886_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { auditValueReply , __4 }
  end | __Stack].

-compile({inline,{yeccpars2_889_,1}}).
-file("megaco_text_parser_v3.yrl", 1569).
yeccpars2_889_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_891_,1}}).
-file("megaco_text_parser_v3.yrl", 994).
yeccpars2_891_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_uint ( __1 , 0 , 999 )
  end | __Stack].

-compile({inline,{yeccpars2_893_,1}}).
-file("megaco_text_parser_v3.yrl", 997).
yeccpars2_893_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_895_,1}}).
-file("megaco_text_parser_v3.yrl", 996).
yeccpars2_895_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   value_of ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_896_,1}}).
-file("megaco_text_parser_v3.yrl", 991).
yeccpars2_896_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'ErrorDescriptor' { errorCode = __3 ,
    errorText = __5 }
  end | __Stack].

-compile({inline,{yeccpars2_897_,1}}).
-file("megaco_text_parser_v3.yrl", 734).
yeccpars2_897_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { error , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_900_,1}}).
-file("megaco_text_parser_v3.yrl", 756).
yeccpars2_900_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { statisticsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_901_,1}}).
-file("megaco_text_parser_v3.yrl", 752).
yeccpars2_901_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { signalsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_902_,1}}).
-file("megaco_text_parser_v3.yrl", 757).
yeccpars2_902_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { packagesDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_903_,1}}).
-file("megaco_text_parser_v3.yrl", 754).
yeccpars2_903_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { observedEventsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_904_,1}}).
-file("megaco_text_parser_v3.yrl", 750).
yeccpars2_904_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { muxDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_905_,1}}).
-file("megaco_text_parser_v3.yrl", 0).
yeccpars2_905_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   '$undefined'
  end | __Stack].

-compile({inline,{yeccpars2_906_,1}}).
-file("megaco_text_parser_v3.yrl", 748).
yeccpars2_906_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { mediaDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_907_,1}}).
-file("megaco_text_parser_v3.yrl", 751).
yeccpars2_907_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { eventsDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_908_,1}}).
-file("megaco_text_parser_v3.yrl", 755).
yeccpars2_908_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { eventBufferDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_909_,1}}).
-file("megaco_text_parser_v3.yrl", 758).
yeccpars2_909_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { errorDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_910_,1}}).
-file("megaco_text_parser_v3.yrl", 753).
yeccpars2_910_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { digitMapDescriptor , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_911_,1}}).
-file("megaco_text_parser_v3.yrl", 746).
yeccpars2_911_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_912_,1}}).
-file("megaco_text_parser_v3.yrl", 759).
yeccpars2_912_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { auditReturnItem , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_913_,1}}).
-file("megaco_text_parser_v3.yrl", 774).
yeccpars2_913_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   mediaToken
  end | __Stack].

-compile({inline,{yeccpars2_914_,1}}).
-file("megaco_text_parser_v3.yrl", 773).
yeccpars2_914_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   modemToken
  end | __Stack].

-compile({inline,{yeccpars2_915_,1}}).
-file("megaco_text_parser_v3.yrl", 772).
yeccpars2_915_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   muxToken
  end | __Stack].

-compile({inline,{yeccpars2_916_,1}}).
-file("megaco_text_parser_v3.yrl", 777).
yeccpars2_916_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   observedEventsToken
  end | __Stack].

-compile({inline,{yeccpars2_917_,1}}).
-file("megaco_text_parser_v3.yrl", 778).
yeccpars2_917_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   packagesToken
  end | __Stack].

-compile({inline,{yeccpars2_918_,1}}).
-file("megaco_text_parser_v3.yrl", 776).
yeccpars2_918_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   statsToken
  end | __Stack].

-compile({inline,{yeccpars2_920_,1}}).
-file("megaco_text_parser_v3.yrl", 1488).
yeccpars2_920_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_923_,1}}).
-file("megaco_text_parser_v3.yrl", 1488).
yeccpars2_923_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_924_,1}}).
-file("megaco_text_parser_v3.yrl", 1487).
yeccpars2_924_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_925_,1}}).
-file("megaco_text_parser_v3.yrl", 1485).
yeccpars2_925_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __3 | __4 ]
  end | __Stack].

-compile({inline,{yeccpars2_926_,1}}).
-file("megaco_text_parser_v3.yrl", 743).
yeccpars2_926_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   merge_terminationAudit ( [ __1 | __2 ] )
  end | __Stack].

-compile({inline,{yeccpars2_928_,1}}).
-file("megaco_text_parser_v3.yrl", 746).
yeccpars2_928_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_929_,1}}).
-file("megaco_text_parser_v3.yrl", 745).
yeccpars2_929_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_930_,1}}).
-file("megaco_text_parser_v3.yrl", 739).
yeccpars2_930_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   merge_auditOther ( __1 , __3 )
  end | __Stack].

-compile({inline,{yeccpars2_932_,1}}).
-file("megaco_text_parser_v3.yrl", 729).
yeccpars2_932_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { auditCapReply , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_933_,1}}).
-file("megaco_text_parser_v3.yrl", 1554).
yeccpars2_933_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   make_safe_token ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_934_,1}}).
-file("megaco_text_parser_v3.yrl", 725).
yeccpars2_934_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { auditCapReply , __4 }
  end | __Stack].

-compile({inline,{yeccpars2_935_,1}}).
-file("megaco_text_parser_v3.yrl", 641).
yeccpars2_935_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   setelement ( # 'ActionReply' .contextId , __5 , __3 )
  end | __Stack].

-compile({inline,{yeccpars2_937_,1}}).
-file("megaco_text_parser_v3.yrl", 704).
yeccpars2_937_(__Stack0) ->
 [begin
   asn1_NOVALUE
  end | __Stack0].

-compile({inline,{yeccpars2_938_,1}}).
-file("megaco_text_parser_v3.yrl", 695).
yeccpars2_938_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , # 'AmmsReply' { terminationID = __3 ,
    terminationAudit = __4 } }
  end | __Stack].

-compile({inline,{yeccpars2_941_,1}}).
-file("megaco_text_parser_v3.yrl", 703).
yeccpars2_941_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_942_,1}}).
-file("megaco_text_parser_v3.yrl", 648).
yeccpars2_942_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   merge_action_reply ( [ __1 | __2 ] )
  end | __Stack].

-compile({inline,{yeccpars2_944_,1}}).
-file("megaco_text_parser_v3.yrl", 655).
yeccpars2_944_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ { error , __2 } ]
  end | __Stack].

-compile({inline,{yeccpars2_945_,1}}).
-file("megaco_text_parser_v3.yrl", 658).
yeccpars2_945_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_946_,1}}).
-file("megaco_text_parser_v3.yrl", 657).
yeccpars2_946_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_947_,1}}).
-file("megaco_text_parser_v3.yrl", 634).
yeccpars2_947_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { actionReplies , [ __1 | __2 ] }
  end | __Stack].

-compile({inline,{yeccpars2_949_,1}}).
-file("megaco_text_parser_v3.yrl", 637).
yeccpars2_949_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,{yeccpars2_950_,1}}).
-file("megaco_text_parser_v3.yrl", 636).
yeccpars2_950_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_951_,1}}).
-file("megaco_text_parser_v3.yrl", 622).
yeccpars2_951_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   make_TransactionReply ( __3 , __5 , __6 )
  end | __Stack].

-compile({inline,{yeccpars2_955_,1}}).
-file("megaco_text_parser_v3.yrl", 517).
yeccpars2_955_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   # 'TransactionPending' { transactionId = ensure_transactionID ( __3 ) }
  end | __Stack].

-compile({inline,{yeccpars2_957_,1}}).
-file("megaco_text_parser_v3.yrl", 626).
yeccpars2_957_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   make_SegmentReply ( __3 )
  end | __Stack].

-compile({inline,{yeccpars2_958_,1}}).
-file("megaco_text_parser_v3.yrl", 498).
yeccpars2_958_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __2 ]
  end | __Stack].

-compile({inline,{yeccpars2_959_,1}}).
-file("megaco_text_parser_v3.yrl", 1044).
yeccpars2_959_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_pathName ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_960_,1}}).
-file("megaco_text_parser_v3.yrl", 1013).
yeccpars2_960_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { deviceName , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_961_,1}}).
-file("megaco_text_parser_v3.yrl", 484).
yeccpars2_961_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_962_,1}}).
-file("megaco_text_parser_v3.yrl", 484).
yeccpars2_962_(__Stack0) ->
 [begin
   no_sep
  end | __Stack0].

-compile({inline,{yeccpars2_963_,1}}).
-file("megaco_text_parser_v3.yrl", 1030).
yeccpars2_963_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ensure_mtpAddress ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_964_,1}}).
-file("megaco_text_parser_v3.yrl", 1006).
yeccpars2_964_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_965_,1}}).
-file("megaco_text_parser_v3.yrl", 1005).
yeccpars2_965_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].


-file("megaco_text_parser_v3.yrl", 1675).
