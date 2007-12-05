-module(core_parse).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("core_parse.yrl", 373).

-export([abstract/1,abstract/2,normalise/1]).

%% The following directive is needed for (significantly) faster compilation
%% of the generated .erl file by the HiPE compiler.  Please do not remove.
-compile([{hipe,[{regalloc,linear_scan}]}]).

-include("core_parse.hrl").

tok_val(T) -> element(3, T).
tok_line(T) -> element(2, T).

abstract(T, _N) -> abstract(T).

abstract(Term) -> core_lib:make_literal(Term).

normalise(Core) -> core_lib:literal_value(Core).

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



-file("./core_parse.erl", 166).

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
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(151=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(152=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(160=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(162=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(163=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_172(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(173=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_191(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(192=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(193=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(194=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_194(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(195=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_195(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(196=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(197=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_197(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(198=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(199=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_191(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(200=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(201=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(202=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(203=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(204=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(205=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(206=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(207=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_191(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(208=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(209=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_191(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(210=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_210(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(211=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_211(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(212=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_191(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(229=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(230=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(231=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_191(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(232=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_232(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(241=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_241(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(242=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(251=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(252=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_252(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(253=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_253(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(254=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(255=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_255(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(256=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(257=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_257(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(258=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(267=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(268=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_268(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(269=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(270=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_270(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(271=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_271(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(272=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_272(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(273=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(274=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_274(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(275=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(276=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_276(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(277=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_277(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(278=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_271(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(292=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_292(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(293=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(294=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_294(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(295=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_295(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(296=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_304(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(305=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_305(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(306=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_306(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(307=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(308=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_308(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(309=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_309(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(310=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(317=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(318=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_318(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(319=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_319(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(320=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_320(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(321=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(322=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_322(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(323=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_323(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.2",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 2, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, module, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr).

yeccpars2_1(_S, '$end', _Ss, Stack,  _T, _Ts, _Tzr) ->
 {ok, hd(Stack)}.

yeccpars2_2(S, module, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 315, Ss, Stack, T, Ts, Tzr).

yeccpars2_3(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr).

yeccpars2_4(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr).

yeccpars2_5(S, attributes, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr).

yeccpars2_6(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 10, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr).

yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_exported_name(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_8(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr).

yeccpars2_9(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 14, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_9_(Stack),
 yeccpars2(yeccgoto_exported_names(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_module_export(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_11(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr).

yeccpars2_12(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr).

yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_13_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_function_name(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_14(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr).

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_15_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_exported_names(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_16_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_module_export(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_17(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_17_(Stack),
 yeccpars2(58, Cat, [17 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_18(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr).

yeccpars2_19(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr).

yeccpars2_20(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr).

yeccpars2_21(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_21_(Stack),
 yeccpars2(yeccgoto_attribute_list(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_22_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_module_attribute(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_23(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr).

yeccpars2_24(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr).

yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_literal(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_26_(Stack),
 yeccpars2(yeccgoto_atomic_literal(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_27_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_attribute(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_literal(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_literal(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_30(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_30(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr).

yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_(Stack),
 yeccpars2(yeccgoto_atomic_literal(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_32_(Stack),
 yeccpars2(yeccgoto_atomic_literal(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_33_(Stack),
 yeccpars2(yeccgoto_atomic_literal(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_34_(Stack),
 yeccpars2(yeccgoto_atomic_literal(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_35_(Stack),
 yeccpars2(yeccgoto_atomic_literal(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_36(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr).

yeccpars2_37(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr).

yeccpars2_38(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_38_(Stack),
 yeccpars2(yeccgoto_literals(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_39_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_tuple_literal(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_40: see yeccpars2_24

yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_41_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_literals(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_42_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_tuple_literal(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_43(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_43(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr).

yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_44_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_nil(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_45_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_cons_literal(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_46: see yeccpars2_24

yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_47_(Stack),
 yeccpars2(yeccgoto_tail_literal(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_48: see yeccpars2_24

yeccpars2_49(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr).

yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_50_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_tail_literal(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_51: see yeccpars2_43

yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_52_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_tail_literal(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_53(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr).

yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_54_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_attribute_list(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_55_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_module_attribute(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_56(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 314, Ss, Stack, T, Ts, Tzr).

yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_anno_function_name(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_module_defs(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_59(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_59_(Stack),
 yeccpars2(313, Cat, [59 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_60(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 96, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_61: see yeccpars2_14

yeccpars2_62(S, '-|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr).

yeccpars2_63(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr).

yeccpars2_64(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr).

yeccpars2_65(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 73, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_constant(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_67_(Stack),
 yeccpars2(yeccgoto_atomic_constant(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_68(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr).

yeccpars2_69(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 92, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_69_(Stack),
 yeccpars2(yeccgoto_constants(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_constant(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_constant(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_72(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_73_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_annotation(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_74_(Stack),
 yeccpars2(yeccgoto_atomic_constant(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_75_(Stack),
 yeccpars2(yeccgoto_atomic_constant(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_76_(Stack),
 yeccpars2(yeccgoto_atomic_constant(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_77_(Stack),
 yeccpars2(yeccgoto_atomic_constant(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_78_(Stack),
 yeccpars2(yeccgoto_atomic_constant(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_79(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr).

yeccpars2_80(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr).

yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_81_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_tuple_constant(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_82_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_tuple_constant(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_83(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 87, Ss, Stack, T, Ts, Tzr).

yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_84_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_cons_constant(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_85(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 76, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_85(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 79, Ss, Stack, T, Ts, Tzr).

yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_86_(Stack),
 yeccpars2(yeccgoto_tail_constant(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_87: see yeccpars2_85

yeccpars2_88(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr).

yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_89_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_tail_constant(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_90: see yeccpars2_83

yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_91_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_tail_constant(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_92: see yeccpars2_85

yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_93_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_constants(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_94_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_annotation(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_95_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_anno_function_name(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_96(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 99, Ss, Stack, T, Ts, Tzr);
yeccpars2_96(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr).

yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_anno_fun(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_98_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_function_definition(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_99(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr).

yeccpars2_100(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr).

yeccpars2_101(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr).

yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_anno_variable(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_103(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 306, Ss, Stack, T, Ts, Tzr).

yeccpars2_104(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 304, Ss, Stack, T, Ts, Tzr);
yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_104_(Stack),
 yeccpars2(yeccgoto_anno_variables(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_105(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr).

yeccpars2_106(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 108, Ss, Stack, T, Ts, Tzr).

yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_107_(Stack),
 yeccpars2(yeccgoto_variable(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_108(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 129, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, call, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, do, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, letrec, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, primop, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 143, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 144, Ss, Stack, T, Ts, Tzr).

yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_single_expression(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_single_expression(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_111(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_single_expression(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expression(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_single_expression(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_single_expression(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_single_expression(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_single_expression(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_single_expression(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_single_expression(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_single_expression(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_anno_expression(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_single_expression(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_single_expression(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_single_expression(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_single_expression(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_single_expression(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_single_expression(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_single_expression(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_128(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_128_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_fun_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_129(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 287, Ss, Stack, T, Ts, Tzr).

yeccpars2_130(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 129, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, call, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, do, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, letrec, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, primop, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 143, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_130(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 144, Ss, Stack, T, Ts, Tzr).

yeccpars2_131(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 129, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 281, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, call, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, do, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, letrec, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, primop, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 143, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_131(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 144, Ss, Stack, T, Ts, Tzr).

yeccpars2_132(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 129, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, call, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, do, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, letrec, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, primop, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 143, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_132(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 144, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_133: see yeccpars2_108

yeccpars2_134(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_134_(Stack),
 yeccpars2(yeccgoto_atomic_literal(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_135: see yeccpars2_108

%% yeccpars2_136: see yeccpars2_108

%% yeccpars2_137: see yeccpars2_108

%% yeccpars2_138: see yeccpars2_108

yeccpars2_139(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr);
yeccpars2_139(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr).

yeccpars2_140(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_140_(Stack),
 yeccpars2(250, Cat, [140 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_141(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 244, Ss, Stack, T, Ts, Tzr).

yeccpars2_142(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(S, 'after', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_142(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_143: see yeccpars2_108

yeccpars2_144(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 129, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, call, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, do, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, letrec, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, primop, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 143, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 144, Ss, Stack, T, Ts, Tzr);
yeccpars2_144(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 147, Ss, Stack, T, Ts, Tzr).

yeccpars2_145(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 150, Ss, Stack, T, Ts, Tzr).

yeccpars2_146(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_146_(Stack),
 yeccpars2(yeccgoto_anno_expressions(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_147(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_147_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_tuple(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_148: see yeccpars2_108

yeccpars2_149(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_149_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_anno_expressions(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_tuple(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_151(S, 'of', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 152, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_152: see yeccpars2_139

yeccpars2_153(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr).

yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_154_(Stack),
 yeccpars2(yeccgoto_let_vars(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_155(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_155(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 157, Ss, Stack, T, Ts, Tzr);
yeccpars2_155(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr).

yeccpars2_156(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr).

yeccpars2_157(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_157_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_let_vars(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_158_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_let_vars(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_159: see yeccpars2_108

yeccpars2_160(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_161: see yeccpars2_139

yeccpars2_162(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_163: see yeccpars2_108

yeccpars2_164(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_164_(Stack),
 Nss = lists:nthtail(9, Ss),
 yeccpars2(yeccgoto_try_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_165(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_other_pattern(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_166(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_166_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_receive_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_anno_pattern(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_other_pattern(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_169(S, 'when', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 240, Ss, Stack, T, Ts, Tzr).

yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_anno_clause(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_other_pattern(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_other_pattern(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_atomic_pattern(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_174(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_anno_pattern(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_175(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_175_(Stack),
 yeccpars2(yeccgoto_clause_pattern(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_176(S, 'after', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 182, Ss, Stack, T, Ts, Tzr).

yeccpars2_177(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_177_(Stack),
 yeccpars2(yeccgoto_anno_clauses(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_178(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 222, Ss, Stack, T, Ts, Tzr).

yeccpars2_179(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr).

yeccpars2_180(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 215, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_180(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr).

yeccpars2_181(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_182: see yeccpars2_108

yeccpars2_183(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_183(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr).

yeccpars2_184(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 201, Ss, Stack, T, Ts, Tzr).

yeccpars2_185(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 199, Ss, Stack, T, Ts, Tzr);
yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_185_(Stack),
 yeccpars2(yeccgoto_anno_patterns(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_186(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_186(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr).

yeccpars2_187(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_187_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_tuple_pattern(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_188(S, '-|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_anno_variable(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_189(S, '-|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr).

yeccpars2_190(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 191, Ss, Stack, T, Ts, Tzr).

yeccpars2_191(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr).

yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_192_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_other_pattern(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_193: see yeccpars2_63

yeccpars2_194(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr).

yeccpars2_195(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_195_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_anno_pattern(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_196: see yeccpars2_63

yeccpars2_197(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr).

yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_198_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_anno_variable(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_199: see yeccpars2_191

yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_200_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_anno_patterns(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_201_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_tuple_pattern(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_202(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_203: see yeccpars2_108

yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_204_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_timeout(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_205(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 207, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 208, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 209, Ss, Stack, T, Ts, Tzr).

yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_206_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_cons_pattern(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_207: see yeccpars2_191

yeccpars2_208(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_208_(Stack),
 yeccpars2(yeccgoto_tail_pattern(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_209: see yeccpars2_191

yeccpars2_210(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 211, Ss, Stack, T, Ts, Tzr).

yeccpars2_211(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_211_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_tail_pattern(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_212: see yeccpars2_205

yeccpars2_213(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_213_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_tail_pattern(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_214(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 216, Ss, Stack, T, Ts, Tzr).

yeccpars2_215(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_215_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_clause_pattern(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_216(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_216_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_clause_pattern(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_217(S, '-|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_anno_pattern(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_218(S, '-|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 219, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_219: see yeccpars2_63

yeccpars2_220(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 221, Ss, Stack, T, Ts, Tzr).

yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_221_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_anno_clause(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_222(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 225, Ss, Stack, T, Ts, Tzr);
yeccpars2_222(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 226, Ss, Stack, T, Ts, Tzr).

yeccpars2_223(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 236, Ss, Stack, T, Ts, Tzr).

yeccpars2_224(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 234, Ss, Stack, T, Ts, Tzr);
yeccpars2_224(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_224_(Stack),
 yeccpars2(yeccgoto_segment_patterns(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_225(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 228, Ss, Stack, T, Ts, Tzr).

yeccpars2_226(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 227, Ss, Stack, T, Ts, Tzr).

yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_227_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_binary_pattern(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_228: see yeccpars2_191

yeccpars2_229(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 230, Ss, Stack, T, Ts, Tzr).

yeccpars2_230(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 231, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_231: see yeccpars2_191

yeccpars2_232(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 233, Ss, Stack, T, Ts, Tzr).

yeccpars2_233(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_233_(Stack),
 Nss = lists:nthtail(6, Ss),
 yeccpars2(yeccgoto_segment_pattern(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_234(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 225, Ss, Stack, T, Ts, Tzr).

yeccpars2_235(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_235_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_segment_patterns(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_236(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 237, Ss, Stack, T, Ts, Tzr).

yeccpars2_237(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_237_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_binary_pattern(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_238(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_238_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_anno_clauses(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_239(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_239_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_receive_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_240: see yeccpars2_108

yeccpars2_241(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 242, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_242: see yeccpars2_108

yeccpars2_243(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_243_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_clause(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_244(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 246, Ss, Stack, T, Ts, Tzr).

yeccpars2_245(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_245_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_primop_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_246(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 129, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 130, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 248, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 131, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 132, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(S, apply, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 133, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 134, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(S, call, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 135, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(S, 'case', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 136, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(S, 'catch', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 137, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(S, do, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 138, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(S, 'fun', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 100, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(S, 'let', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 139, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(S, letrec, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 140, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(S, primop, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 141, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(S, 'receive', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 142, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(S, 'try', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 143, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_246(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 144, Ss, Stack, T, Ts, Tzr).

yeccpars2_247(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 249, Ss, Stack, T, Ts, Tzr).

yeccpars2_248(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_248_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_arg_list(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_249(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_249_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_arg_list(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_250(S, in, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 251, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_251: see yeccpars2_108

yeccpars2_252(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_252_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_letrec_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_253(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 254, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_254: see yeccpars2_108

yeccpars2_255(S, in, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 256, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_256: see yeccpars2_108

yeccpars2_257(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_257_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_let_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_258: see yeccpars2_108

yeccpars2_259(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_259_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_sequence(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_260(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_260_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_catch_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_261(S, 'of', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 262, Ss, Stack, T, Ts, Tzr).

yeccpars2_262(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 178, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(S, char, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(S, float, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr).

yeccpars2_263(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 264, Ss, Stack, T, Ts, Tzr).

yeccpars2_264(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_264_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_case_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_265(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 266, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_266: see yeccpars2_108

%% yeccpars2_267: see yeccpars2_244

yeccpars2_268(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_268_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_call_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_269: see yeccpars2_244

yeccpars2_270(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_270_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_application_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_271(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 273, Ss, Stack, T, Ts, Tzr);
yeccpars2_271(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 274, Ss, Stack, T, Ts, Tzr);
yeccpars2_271(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 275, Ss, Stack, T, Ts, Tzr).

yeccpars2_272(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_272_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_cons(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_273: see yeccpars2_108

yeccpars2_274(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_274_(Stack),
 yeccpars2(yeccgoto_tail(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_275: see yeccpars2_108

yeccpars2_276(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 277, Ss, Stack, T, Ts, Tzr).

yeccpars2_277(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_277_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_tail(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_278: see yeccpars2_271

yeccpars2_279(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_279_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_tail(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_280(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 282, Ss, Stack, T, Ts, Tzr).

yeccpars2_281(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_281_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_expression(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_282(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_282_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_expression(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_283(S, '-|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 284, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_284: see yeccpars2_63

yeccpars2_285(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 286, Ss, Stack, T, Ts, Tzr).

yeccpars2_286(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_286_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_anno_expression(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_287(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 290, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 291, Ss, Stack, T, Ts, Tzr).

yeccpars2_288(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 301, Ss, Stack, T, Ts, Tzr).

yeccpars2_289(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 299, Ss, Stack, T, Ts, Tzr);
yeccpars2_289(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_289_(Stack),
 yeccpars2(yeccgoto_segments(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_290(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 293, Ss, Stack, T, Ts, Tzr).

yeccpars2_291(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 292, Ss, Stack, T, Ts, Tzr).

yeccpars2_292(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_292_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_binary(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_293: see yeccpars2_108

yeccpars2_294(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 295, Ss, Stack, T, Ts, Tzr).

yeccpars2_295(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 296, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_296: see yeccpars2_108

yeccpars2_297(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 298, Ss, Stack, T, Ts, Tzr).

yeccpars2_298(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_298_(Stack),
 Nss = lists:nthtail(6, Ss),
 yeccpars2(yeccgoto_segment(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_299(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 290, Ss, Stack, T, Ts, Tzr).

yeccpars2_300(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_300_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_segments(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_301(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 302, Ss, Stack, T, Ts, Tzr).

yeccpars2_302(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_302_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_binary(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_303(S, '-|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 196, Ss, Stack, T, Ts, Tzr).

yeccpars2_304(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 105, Ss, Stack, T, Ts, Tzr);
yeccpars2_304(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr).

yeccpars2_305(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_305_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_anno_variables(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_306(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 307, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_307: see yeccpars2_108

yeccpars2_308(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_308_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_fun_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_309(S, '-|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 310, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_310: see yeccpars2_63

yeccpars2_311(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 312, Ss, Stack, T, Ts, Tzr).

yeccpars2_312(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_312_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_anno_fun(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_313(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_313_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_function_definitions(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_314(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_314_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_module_definition(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_315(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 316, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_316: see yeccpars2_4

%% yeccpars2_317: see yeccpars2_5

yeccpars2_318(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_318(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_318(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_318_(Stack),
 yeccpars2(58, Cat, [318 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_319(S, 'end', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 320, Ss, Stack, T, Ts, Tzr).

yeccpars2_320(S, '-|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 321, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_321: see yeccpars2_63

yeccpars2_322(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 323, Ss, Stack, T, Ts, Tzr).

yeccpars2_323(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_323_(Stack),
 Nss = lists:nthtail(9, Ss),
 yeccpars2(yeccgoto_module_definition(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccgoto_anno_clause(142) -> 177;
yeccgoto_anno_clause(177) -> 177;
yeccgoto_anno_clause(262) -> 177.

yeccgoto_anno_clauses(142) -> 176;
yeccgoto_anno_clauses(177) -> 238;
yeccgoto_anno_clauses(262) -> 263.

yeccgoto_anno_expression(108) -> 128;
yeccgoto_anno_expression(131) -> 146;
yeccgoto_anno_expression(132) -> 271;
yeccgoto_anno_expression(133) -> 269;
yeccgoto_anno_expression(135) -> 265;
yeccgoto_anno_expression(136) -> 261;
yeccgoto_anno_expression(137) -> 260;
yeccgoto_anno_expression(138) -> 258;
yeccgoto_anno_expression(143) -> 151;
yeccgoto_anno_expression(144) -> 146;
yeccgoto_anno_expression(148) -> 146;
yeccgoto_anno_expression(159) -> 160;
yeccgoto_anno_expression(163) -> 164;
yeccgoto_anno_expression(182) -> 202;
yeccgoto_anno_expression(203) -> 204;
yeccgoto_anno_expression(240) -> 241;
yeccgoto_anno_expression(242) -> 243;
yeccgoto_anno_expression(246) -> 146;
yeccgoto_anno_expression(251) -> 252;
yeccgoto_anno_expression(254) -> 255;
yeccgoto_anno_expression(256) -> 257;
yeccgoto_anno_expression(258) -> 259;
yeccgoto_anno_expression(266) -> 267;
yeccgoto_anno_expression(273) -> 278;
yeccgoto_anno_expression(275) -> 276;
yeccgoto_anno_expression(293) -> 294;
yeccgoto_anno_expression(296) -> 146;
yeccgoto_anno_expression(307) -> 308.

yeccgoto_anno_expressions(131) -> 280;
yeccgoto_anno_expressions(144) -> 145;
yeccgoto_anno_expressions(148) -> 149;
yeccgoto_anno_expressions(246) -> 247;
yeccgoto_anno_expressions(296) -> 297.

yeccgoto_anno_fun(96) -> 98.

yeccgoto_anno_function_name(17) -> 60;
yeccgoto_anno_function_name(59) -> 60;
yeccgoto_anno_function_name(140) -> 60;
yeccgoto_anno_function_name(318) -> 60.

yeccgoto_anno_pattern(142) -> 175;
yeccgoto_anno_pattern(177) -> 175;
yeccgoto_anno_pattern(179) -> 175;
yeccgoto_anno_pattern(180) -> 185;
yeccgoto_anno_pattern(181) -> 205;
yeccgoto_anno_pattern(183) -> 185;
yeccgoto_anno_pattern(191) -> 192;
yeccgoto_anno_pattern(199) -> 185;
yeccgoto_anno_pattern(207) -> 212;
yeccgoto_anno_pattern(209) -> 210;
yeccgoto_anno_pattern(228) -> 229;
yeccgoto_anno_pattern(231) -> 185;
yeccgoto_anno_pattern(262) -> 175.

yeccgoto_anno_patterns(180) -> 214;
yeccgoto_anno_patterns(183) -> 184;
yeccgoto_anno_patterns(199) -> 200;
yeccgoto_anno_patterns(231) -> 232.

yeccgoto_anno_variable(101) -> 104;
yeccgoto_anno_variable(139) -> 154;
yeccgoto_anno_variable(142) -> 174;
yeccgoto_anno_variable(152) -> 154;
yeccgoto_anno_variable(155) -> 104;
yeccgoto_anno_variable(161) -> 154;
yeccgoto_anno_variable(177) -> 174;
yeccgoto_anno_variable(179) -> 174;
yeccgoto_anno_variable(180) -> 174;
yeccgoto_anno_variable(181) -> 174;
yeccgoto_anno_variable(183) -> 174;
yeccgoto_anno_variable(186) -> 190;
yeccgoto_anno_variable(191) -> 174;
yeccgoto_anno_variable(199) -> 174;
yeccgoto_anno_variable(207) -> 174;
yeccgoto_anno_variable(209) -> 174;
yeccgoto_anno_variable(228) -> 174;
yeccgoto_anno_variable(231) -> 174;
yeccgoto_anno_variable(262) -> 174;
yeccgoto_anno_variable(304) -> 104.

yeccgoto_anno_variables(101) -> 103;
yeccgoto_anno_variables(155) -> 156;
yeccgoto_anno_variables(304) -> 305.

yeccgoto_annotation(63) -> 64;
yeccgoto_annotation(193) -> 194;
yeccgoto_annotation(196) -> 197;
yeccgoto_annotation(219) -> 220;
yeccgoto_annotation(284) -> 285;
yeccgoto_annotation(310) -> 311;
yeccgoto_annotation(321) -> 322.

yeccgoto_application_expr(108) -> 127;
yeccgoto_application_expr(130) -> 127;
yeccgoto_application_expr(131) -> 127;
yeccgoto_application_expr(132) -> 127;
yeccgoto_application_expr(133) -> 127;
yeccgoto_application_expr(135) -> 127;
yeccgoto_application_expr(136) -> 127;
yeccgoto_application_expr(137) -> 127;
yeccgoto_application_expr(138) -> 127;
yeccgoto_application_expr(143) -> 127;
yeccgoto_application_expr(144) -> 127;
yeccgoto_application_expr(148) -> 127;
yeccgoto_application_expr(159) -> 127;
yeccgoto_application_expr(163) -> 127;
yeccgoto_application_expr(182) -> 127;
yeccgoto_application_expr(203) -> 127;
yeccgoto_application_expr(240) -> 127;
yeccgoto_application_expr(242) -> 127;
yeccgoto_application_expr(246) -> 127;
yeccgoto_application_expr(251) -> 127;
yeccgoto_application_expr(254) -> 127;
yeccgoto_application_expr(256) -> 127;
yeccgoto_application_expr(258) -> 127;
yeccgoto_application_expr(266) -> 127;
yeccgoto_application_expr(273) -> 127;
yeccgoto_application_expr(275) -> 127;
yeccgoto_application_expr(293) -> 127;
yeccgoto_application_expr(296) -> 127;
yeccgoto_application_expr(307) -> 127.

yeccgoto_arg_list(244) -> 245;
yeccgoto_arg_list(267) -> 268;
yeccgoto_arg_list(269) -> 270.

yeccgoto_atomic_constant(65) -> 71;
yeccgoto_atomic_constant(72) -> 71;
yeccgoto_atomic_constant(79) -> 71;
yeccgoto_atomic_constant(85) -> 71;
yeccgoto_atomic_constant(87) -> 71;
yeccgoto_atomic_constant(92) -> 71.

yeccgoto_atomic_literal(24) -> 29;
yeccgoto_atomic_literal(30) -> 29;
yeccgoto_atomic_literal(36) -> 29;
yeccgoto_atomic_literal(40) -> 29;
yeccgoto_atomic_literal(46) -> 29;
yeccgoto_atomic_literal(48) -> 29;
yeccgoto_atomic_literal(108) -> 126;
yeccgoto_atomic_literal(130) -> 126;
yeccgoto_atomic_literal(131) -> 126;
yeccgoto_atomic_literal(132) -> 126;
yeccgoto_atomic_literal(133) -> 126;
yeccgoto_atomic_literal(135) -> 126;
yeccgoto_atomic_literal(136) -> 126;
yeccgoto_atomic_literal(137) -> 126;
yeccgoto_atomic_literal(138) -> 126;
yeccgoto_atomic_literal(142) -> 173;
yeccgoto_atomic_literal(143) -> 126;
yeccgoto_atomic_literal(144) -> 126;
yeccgoto_atomic_literal(148) -> 126;
yeccgoto_atomic_literal(159) -> 126;
yeccgoto_atomic_literal(163) -> 126;
yeccgoto_atomic_literal(177) -> 173;
yeccgoto_atomic_literal(179) -> 173;
yeccgoto_atomic_literal(180) -> 173;
yeccgoto_atomic_literal(181) -> 173;
yeccgoto_atomic_literal(182) -> 126;
yeccgoto_atomic_literal(183) -> 173;
yeccgoto_atomic_literal(186) -> 173;
yeccgoto_atomic_literal(191) -> 173;
yeccgoto_atomic_literal(199) -> 173;
yeccgoto_atomic_literal(203) -> 126;
yeccgoto_atomic_literal(207) -> 173;
yeccgoto_atomic_literal(209) -> 173;
yeccgoto_atomic_literal(228) -> 173;
yeccgoto_atomic_literal(231) -> 173;
yeccgoto_atomic_literal(240) -> 126;
yeccgoto_atomic_literal(242) -> 126;
yeccgoto_atomic_literal(246) -> 126;
yeccgoto_atomic_literal(251) -> 126;
yeccgoto_atomic_literal(254) -> 126;
yeccgoto_atomic_literal(256) -> 126;
yeccgoto_atomic_literal(258) -> 126;
yeccgoto_atomic_literal(262) -> 173;
yeccgoto_atomic_literal(266) -> 126;
yeccgoto_atomic_literal(273) -> 126;
yeccgoto_atomic_literal(275) -> 126;
yeccgoto_atomic_literal(293) -> 126;
yeccgoto_atomic_literal(296) -> 126;
yeccgoto_atomic_literal(307) -> 126.

yeccgoto_atomic_pattern(142) -> 172;
yeccgoto_atomic_pattern(177) -> 172;
yeccgoto_atomic_pattern(179) -> 172;
yeccgoto_atomic_pattern(180) -> 172;
yeccgoto_atomic_pattern(181) -> 172;
yeccgoto_atomic_pattern(183) -> 172;
yeccgoto_atomic_pattern(186) -> 172;
yeccgoto_atomic_pattern(191) -> 172;
yeccgoto_atomic_pattern(199) -> 172;
yeccgoto_atomic_pattern(207) -> 172;
yeccgoto_atomic_pattern(209) -> 172;
yeccgoto_atomic_pattern(228) -> 172;
yeccgoto_atomic_pattern(231) -> 172;
yeccgoto_atomic_pattern(262) -> 172.

yeccgoto_attribute(19) -> 21;
yeccgoto_attribute(53) -> 21.

yeccgoto_attribute_list(19) -> 20;
yeccgoto_attribute_list(53) -> 54.

yeccgoto_binary(108) -> 125;
yeccgoto_binary(130) -> 125;
yeccgoto_binary(131) -> 125;
yeccgoto_binary(132) -> 125;
yeccgoto_binary(133) -> 125;
yeccgoto_binary(135) -> 125;
yeccgoto_binary(136) -> 125;
yeccgoto_binary(137) -> 125;
yeccgoto_binary(138) -> 125;
yeccgoto_binary(143) -> 125;
yeccgoto_binary(144) -> 125;
yeccgoto_binary(148) -> 125;
yeccgoto_binary(159) -> 125;
yeccgoto_binary(163) -> 125;
yeccgoto_binary(182) -> 125;
yeccgoto_binary(203) -> 125;
yeccgoto_binary(240) -> 125;
yeccgoto_binary(242) -> 125;
yeccgoto_binary(246) -> 125;
yeccgoto_binary(251) -> 125;
yeccgoto_binary(254) -> 125;
yeccgoto_binary(256) -> 125;
yeccgoto_binary(258) -> 125;
yeccgoto_binary(266) -> 125;
yeccgoto_binary(273) -> 125;
yeccgoto_binary(275) -> 125;
yeccgoto_binary(293) -> 125;
yeccgoto_binary(296) -> 125;
yeccgoto_binary(307) -> 125.

yeccgoto_binary_pattern(142) -> 171;
yeccgoto_binary_pattern(177) -> 171;
yeccgoto_binary_pattern(179) -> 171;
yeccgoto_binary_pattern(180) -> 171;
yeccgoto_binary_pattern(181) -> 171;
yeccgoto_binary_pattern(183) -> 171;
yeccgoto_binary_pattern(186) -> 171;
yeccgoto_binary_pattern(191) -> 171;
yeccgoto_binary_pattern(199) -> 171;
yeccgoto_binary_pattern(207) -> 171;
yeccgoto_binary_pattern(209) -> 171;
yeccgoto_binary_pattern(228) -> 171;
yeccgoto_binary_pattern(231) -> 171;
yeccgoto_binary_pattern(262) -> 171.

yeccgoto_call_expr(108) -> 124;
yeccgoto_call_expr(130) -> 124;
yeccgoto_call_expr(131) -> 124;
yeccgoto_call_expr(132) -> 124;
yeccgoto_call_expr(133) -> 124;
yeccgoto_call_expr(135) -> 124;
yeccgoto_call_expr(136) -> 124;
yeccgoto_call_expr(137) -> 124;
yeccgoto_call_expr(138) -> 124;
yeccgoto_call_expr(143) -> 124;
yeccgoto_call_expr(144) -> 124;
yeccgoto_call_expr(148) -> 124;
yeccgoto_call_expr(159) -> 124;
yeccgoto_call_expr(163) -> 124;
yeccgoto_call_expr(182) -> 124;
yeccgoto_call_expr(203) -> 124;
yeccgoto_call_expr(240) -> 124;
yeccgoto_call_expr(242) -> 124;
yeccgoto_call_expr(246) -> 124;
yeccgoto_call_expr(251) -> 124;
yeccgoto_call_expr(254) -> 124;
yeccgoto_call_expr(256) -> 124;
yeccgoto_call_expr(258) -> 124;
yeccgoto_call_expr(266) -> 124;
yeccgoto_call_expr(273) -> 124;
yeccgoto_call_expr(275) -> 124;
yeccgoto_call_expr(293) -> 124;
yeccgoto_call_expr(296) -> 124;
yeccgoto_call_expr(307) -> 124.

yeccgoto_case_expr(108) -> 123;
yeccgoto_case_expr(130) -> 123;
yeccgoto_case_expr(131) -> 123;
yeccgoto_case_expr(132) -> 123;
yeccgoto_case_expr(133) -> 123;
yeccgoto_case_expr(135) -> 123;
yeccgoto_case_expr(136) -> 123;
yeccgoto_case_expr(137) -> 123;
yeccgoto_case_expr(138) -> 123;
yeccgoto_case_expr(143) -> 123;
yeccgoto_case_expr(144) -> 123;
yeccgoto_case_expr(148) -> 123;
yeccgoto_case_expr(159) -> 123;
yeccgoto_case_expr(163) -> 123;
yeccgoto_case_expr(182) -> 123;
yeccgoto_case_expr(203) -> 123;
yeccgoto_case_expr(240) -> 123;
yeccgoto_case_expr(242) -> 123;
yeccgoto_case_expr(246) -> 123;
yeccgoto_case_expr(251) -> 123;
yeccgoto_case_expr(254) -> 123;
yeccgoto_case_expr(256) -> 123;
yeccgoto_case_expr(258) -> 123;
yeccgoto_case_expr(266) -> 123;
yeccgoto_case_expr(273) -> 123;
yeccgoto_case_expr(275) -> 123;
yeccgoto_case_expr(293) -> 123;
yeccgoto_case_expr(296) -> 123;
yeccgoto_case_expr(307) -> 123.

yeccgoto_catch_expr(108) -> 122;
yeccgoto_catch_expr(130) -> 122;
yeccgoto_catch_expr(131) -> 122;
yeccgoto_catch_expr(132) -> 122;
yeccgoto_catch_expr(133) -> 122;
yeccgoto_catch_expr(135) -> 122;
yeccgoto_catch_expr(136) -> 122;
yeccgoto_catch_expr(137) -> 122;
yeccgoto_catch_expr(138) -> 122;
yeccgoto_catch_expr(143) -> 122;
yeccgoto_catch_expr(144) -> 122;
yeccgoto_catch_expr(148) -> 122;
yeccgoto_catch_expr(159) -> 122;
yeccgoto_catch_expr(163) -> 122;
yeccgoto_catch_expr(182) -> 122;
yeccgoto_catch_expr(203) -> 122;
yeccgoto_catch_expr(240) -> 122;
yeccgoto_catch_expr(242) -> 122;
yeccgoto_catch_expr(246) -> 122;
yeccgoto_catch_expr(251) -> 122;
yeccgoto_catch_expr(254) -> 122;
yeccgoto_catch_expr(256) -> 122;
yeccgoto_catch_expr(258) -> 122;
yeccgoto_catch_expr(266) -> 122;
yeccgoto_catch_expr(273) -> 122;
yeccgoto_catch_expr(275) -> 122;
yeccgoto_catch_expr(293) -> 122;
yeccgoto_catch_expr(296) -> 122;
yeccgoto_catch_expr(307) -> 122.

yeccgoto_clause(142) -> 170;
yeccgoto_clause(177) -> 170;
yeccgoto_clause(179) -> 218;
yeccgoto_clause(262) -> 170.

yeccgoto_clause_pattern(142) -> 169;
yeccgoto_clause_pattern(177) -> 169;
yeccgoto_clause_pattern(179) -> 169;
yeccgoto_clause_pattern(262) -> 169.

yeccgoto_cons(108) -> 121;
yeccgoto_cons(130) -> 121;
yeccgoto_cons(131) -> 121;
yeccgoto_cons(132) -> 121;
yeccgoto_cons(133) -> 121;
yeccgoto_cons(135) -> 121;
yeccgoto_cons(136) -> 121;
yeccgoto_cons(137) -> 121;
yeccgoto_cons(138) -> 121;
yeccgoto_cons(143) -> 121;
yeccgoto_cons(144) -> 121;
yeccgoto_cons(148) -> 121;
yeccgoto_cons(159) -> 121;
yeccgoto_cons(163) -> 121;
yeccgoto_cons(182) -> 121;
yeccgoto_cons(203) -> 121;
yeccgoto_cons(240) -> 121;
yeccgoto_cons(242) -> 121;
yeccgoto_cons(246) -> 121;
yeccgoto_cons(251) -> 121;
yeccgoto_cons(254) -> 121;
yeccgoto_cons(256) -> 121;
yeccgoto_cons(258) -> 121;
yeccgoto_cons(266) -> 121;
yeccgoto_cons(273) -> 121;
yeccgoto_cons(275) -> 121;
yeccgoto_cons(293) -> 121;
yeccgoto_cons(296) -> 121;
yeccgoto_cons(307) -> 121.

yeccgoto_cons_constant(65) -> 70;
yeccgoto_cons_constant(72) -> 70;
yeccgoto_cons_constant(79) -> 70;
yeccgoto_cons_constant(85) -> 70;
yeccgoto_cons_constant(87) -> 70;
yeccgoto_cons_constant(92) -> 70.

yeccgoto_cons_literal(24) -> 28;
yeccgoto_cons_literal(30) -> 28;
yeccgoto_cons_literal(36) -> 28;
yeccgoto_cons_literal(40) -> 28;
yeccgoto_cons_literal(46) -> 28;
yeccgoto_cons_literal(48) -> 28.

yeccgoto_cons_pattern(142) -> 168;
yeccgoto_cons_pattern(177) -> 168;
yeccgoto_cons_pattern(179) -> 168;
yeccgoto_cons_pattern(180) -> 168;
yeccgoto_cons_pattern(181) -> 168;
yeccgoto_cons_pattern(183) -> 168;
yeccgoto_cons_pattern(186) -> 168;
yeccgoto_cons_pattern(191) -> 168;
yeccgoto_cons_pattern(199) -> 168;
yeccgoto_cons_pattern(207) -> 168;
yeccgoto_cons_pattern(209) -> 168;
yeccgoto_cons_pattern(228) -> 168;
yeccgoto_cons_pattern(231) -> 168;
yeccgoto_cons_pattern(262) -> 168.

yeccgoto_constant(65) -> 69;
yeccgoto_constant(72) -> 83;
yeccgoto_constant(79) -> 69;
yeccgoto_constant(85) -> 90;
yeccgoto_constant(87) -> 88;
yeccgoto_constant(92) -> 69.

yeccgoto_constants(65) -> 68;
yeccgoto_constants(79) -> 80;
yeccgoto_constants(92) -> 93.

yeccgoto_exported_name(6) -> 9;
yeccgoto_exported_name(14) -> 9.

yeccgoto_exported_names(6) -> 8;
yeccgoto_exported_names(14) -> 15.

yeccgoto_expression(108) -> 120;
yeccgoto_expression(130) -> 283;
yeccgoto_expression(131) -> 120;
yeccgoto_expression(132) -> 120;
yeccgoto_expression(133) -> 120;
yeccgoto_expression(135) -> 120;
yeccgoto_expression(136) -> 120;
yeccgoto_expression(137) -> 120;
yeccgoto_expression(138) -> 120;
yeccgoto_expression(143) -> 120;
yeccgoto_expression(144) -> 120;
yeccgoto_expression(148) -> 120;
yeccgoto_expression(159) -> 120;
yeccgoto_expression(163) -> 120;
yeccgoto_expression(182) -> 120;
yeccgoto_expression(203) -> 120;
yeccgoto_expression(240) -> 120;
yeccgoto_expression(242) -> 120;
yeccgoto_expression(246) -> 120;
yeccgoto_expression(251) -> 120;
yeccgoto_expression(254) -> 120;
yeccgoto_expression(256) -> 120;
yeccgoto_expression(258) -> 120;
yeccgoto_expression(266) -> 120;
yeccgoto_expression(273) -> 120;
yeccgoto_expression(275) -> 120;
yeccgoto_expression(293) -> 120;
yeccgoto_expression(296) -> 120;
yeccgoto_expression(307) -> 120.

yeccgoto_fun_expr(96) -> 97;
yeccgoto_fun_expr(99) -> 309;
yeccgoto_fun_expr(108) -> 119;
yeccgoto_fun_expr(130) -> 119;
yeccgoto_fun_expr(131) -> 119;
yeccgoto_fun_expr(132) -> 119;
yeccgoto_fun_expr(133) -> 119;
yeccgoto_fun_expr(135) -> 119;
yeccgoto_fun_expr(136) -> 119;
yeccgoto_fun_expr(137) -> 119;
yeccgoto_fun_expr(138) -> 119;
yeccgoto_fun_expr(143) -> 119;
yeccgoto_fun_expr(144) -> 119;
yeccgoto_fun_expr(148) -> 119;
yeccgoto_fun_expr(159) -> 119;
yeccgoto_fun_expr(163) -> 119;
yeccgoto_fun_expr(182) -> 119;
yeccgoto_fun_expr(203) -> 119;
yeccgoto_fun_expr(240) -> 119;
yeccgoto_fun_expr(242) -> 119;
yeccgoto_fun_expr(246) -> 119;
yeccgoto_fun_expr(251) -> 119;
yeccgoto_fun_expr(254) -> 119;
yeccgoto_fun_expr(256) -> 119;
yeccgoto_fun_expr(258) -> 119;
yeccgoto_fun_expr(266) -> 119;
yeccgoto_fun_expr(273) -> 119;
yeccgoto_fun_expr(275) -> 119;
yeccgoto_fun_expr(293) -> 119;
yeccgoto_fun_expr(296) -> 119;
yeccgoto_fun_expr(307) -> 119.

yeccgoto_function_definition(17) -> 59;
yeccgoto_function_definition(59) -> 59;
yeccgoto_function_definition(140) -> 59;
yeccgoto_function_definition(318) -> 59.

yeccgoto_function_definitions(17) -> 58;
yeccgoto_function_definitions(59) -> 313;
yeccgoto_function_definitions(140) -> 250;
yeccgoto_function_definitions(318) -> 58.

yeccgoto_function_name(6) -> 7;
yeccgoto_function_name(14) -> 7;
yeccgoto_function_name(17) -> 57;
yeccgoto_function_name(59) -> 57;
yeccgoto_function_name(61) -> 62;
yeccgoto_function_name(108) -> 118;
yeccgoto_function_name(130) -> 118;
yeccgoto_function_name(131) -> 118;
yeccgoto_function_name(132) -> 118;
yeccgoto_function_name(133) -> 118;
yeccgoto_function_name(135) -> 118;
yeccgoto_function_name(136) -> 118;
yeccgoto_function_name(137) -> 118;
yeccgoto_function_name(138) -> 118;
yeccgoto_function_name(140) -> 57;
yeccgoto_function_name(143) -> 118;
yeccgoto_function_name(144) -> 118;
yeccgoto_function_name(148) -> 118;
yeccgoto_function_name(159) -> 118;
yeccgoto_function_name(163) -> 118;
yeccgoto_function_name(182) -> 118;
yeccgoto_function_name(203) -> 118;
yeccgoto_function_name(240) -> 118;
yeccgoto_function_name(242) -> 118;
yeccgoto_function_name(246) -> 118;
yeccgoto_function_name(251) -> 118;
yeccgoto_function_name(254) -> 118;
yeccgoto_function_name(256) -> 118;
yeccgoto_function_name(258) -> 118;
yeccgoto_function_name(266) -> 118;
yeccgoto_function_name(273) -> 118;
yeccgoto_function_name(275) -> 118;
yeccgoto_function_name(293) -> 118;
yeccgoto_function_name(296) -> 118;
yeccgoto_function_name(307) -> 118;
yeccgoto_function_name(318) -> 57.

yeccgoto_let_expr(108) -> 117;
yeccgoto_let_expr(130) -> 117;
yeccgoto_let_expr(131) -> 117;
yeccgoto_let_expr(132) -> 117;
yeccgoto_let_expr(133) -> 117;
yeccgoto_let_expr(135) -> 117;
yeccgoto_let_expr(136) -> 117;
yeccgoto_let_expr(137) -> 117;
yeccgoto_let_expr(138) -> 117;
yeccgoto_let_expr(143) -> 117;
yeccgoto_let_expr(144) -> 117;
yeccgoto_let_expr(148) -> 117;
yeccgoto_let_expr(159) -> 117;
yeccgoto_let_expr(163) -> 117;
yeccgoto_let_expr(182) -> 117;
yeccgoto_let_expr(203) -> 117;
yeccgoto_let_expr(240) -> 117;
yeccgoto_let_expr(242) -> 117;
yeccgoto_let_expr(246) -> 117;
yeccgoto_let_expr(251) -> 117;
yeccgoto_let_expr(254) -> 117;
yeccgoto_let_expr(256) -> 117;
yeccgoto_let_expr(258) -> 117;
yeccgoto_let_expr(266) -> 117;
yeccgoto_let_expr(273) -> 117;
yeccgoto_let_expr(275) -> 117;
yeccgoto_let_expr(293) -> 117;
yeccgoto_let_expr(296) -> 117;
yeccgoto_let_expr(307) -> 117.

yeccgoto_let_vars(139) -> 253;
yeccgoto_let_vars(152) -> 153;
yeccgoto_let_vars(161) -> 162.

yeccgoto_letrec_expr(108) -> 116;
yeccgoto_letrec_expr(130) -> 116;
yeccgoto_letrec_expr(131) -> 116;
yeccgoto_letrec_expr(132) -> 116;
yeccgoto_letrec_expr(133) -> 116;
yeccgoto_letrec_expr(135) -> 116;
yeccgoto_letrec_expr(136) -> 116;
yeccgoto_letrec_expr(137) -> 116;
yeccgoto_letrec_expr(138) -> 116;
yeccgoto_letrec_expr(143) -> 116;
yeccgoto_letrec_expr(144) -> 116;
yeccgoto_letrec_expr(148) -> 116;
yeccgoto_letrec_expr(159) -> 116;
yeccgoto_letrec_expr(163) -> 116;
yeccgoto_letrec_expr(182) -> 116;
yeccgoto_letrec_expr(203) -> 116;
yeccgoto_letrec_expr(240) -> 116;
yeccgoto_letrec_expr(242) -> 116;
yeccgoto_letrec_expr(246) -> 116;
yeccgoto_letrec_expr(251) -> 116;
yeccgoto_letrec_expr(254) -> 116;
yeccgoto_letrec_expr(256) -> 116;
yeccgoto_letrec_expr(258) -> 116;
yeccgoto_letrec_expr(266) -> 116;
yeccgoto_letrec_expr(273) -> 116;
yeccgoto_letrec_expr(275) -> 116;
yeccgoto_letrec_expr(293) -> 116;
yeccgoto_letrec_expr(296) -> 116;
yeccgoto_letrec_expr(307) -> 116.

yeccgoto_literal(24) -> 27;
yeccgoto_literal(30) -> 43;
yeccgoto_literal(36) -> 38;
yeccgoto_literal(40) -> 38;
yeccgoto_literal(46) -> 51;
yeccgoto_literal(48) -> 49.

yeccgoto_literals(36) -> 37;
yeccgoto_literals(40) -> 41.

yeccgoto_module_attribute(5) -> 17;
yeccgoto_module_attribute(317) -> 318.

yeccgoto_module_definition(0) -> 1.

yeccgoto_module_defs(17) -> 56;
yeccgoto_module_defs(318) -> 319.

yeccgoto_module_export(4) -> 5;
yeccgoto_module_export(316) -> 317.

yeccgoto_nil(24) -> 26;
yeccgoto_nil(30) -> 26;
yeccgoto_nil(36) -> 26;
yeccgoto_nil(40) -> 26;
yeccgoto_nil(46) -> 26;
yeccgoto_nil(48) -> 26;
yeccgoto_nil(65) -> 67;
yeccgoto_nil(72) -> 67;
yeccgoto_nil(79) -> 67;
yeccgoto_nil(85) -> 67;
yeccgoto_nil(87) -> 67;
yeccgoto_nil(92) -> 67;
yeccgoto_nil(108) -> 26;
yeccgoto_nil(130) -> 26;
yeccgoto_nil(131) -> 26;
yeccgoto_nil(132) -> 26;
yeccgoto_nil(133) -> 26;
yeccgoto_nil(135) -> 26;
yeccgoto_nil(136) -> 26;
yeccgoto_nil(137) -> 26;
yeccgoto_nil(138) -> 26;
yeccgoto_nil(142) -> 26;
yeccgoto_nil(143) -> 26;
yeccgoto_nil(144) -> 26;
yeccgoto_nil(148) -> 26;
yeccgoto_nil(159) -> 26;
yeccgoto_nil(163) -> 26;
yeccgoto_nil(177) -> 26;
yeccgoto_nil(179) -> 26;
yeccgoto_nil(180) -> 26;
yeccgoto_nil(181) -> 26;
yeccgoto_nil(182) -> 26;
yeccgoto_nil(183) -> 26;
yeccgoto_nil(186) -> 26;
yeccgoto_nil(191) -> 26;
yeccgoto_nil(199) -> 26;
yeccgoto_nil(203) -> 26;
yeccgoto_nil(207) -> 26;
yeccgoto_nil(209) -> 26;
yeccgoto_nil(228) -> 26;
yeccgoto_nil(231) -> 26;
yeccgoto_nil(240) -> 26;
yeccgoto_nil(242) -> 26;
yeccgoto_nil(246) -> 26;
yeccgoto_nil(251) -> 26;
yeccgoto_nil(254) -> 26;
yeccgoto_nil(256) -> 26;
yeccgoto_nil(258) -> 26;
yeccgoto_nil(262) -> 26;
yeccgoto_nil(266) -> 26;
yeccgoto_nil(273) -> 26;
yeccgoto_nil(275) -> 26;
yeccgoto_nil(293) -> 26;
yeccgoto_nil(296) -> 26;
yeccgoto_nil(307) -> 26.

yeccgoto_other_pattern(142) -> 167;
yeccgoto_other_pattern(177) -> 167;
yeccgoto_other_pattern(179) -> 217;
yeccgoto_other_pattern(180) -> 167;
yeccgoto_other_pattern(181) -> 167;
yeccgoto_other_pattern(183) -> 167;
yeccgoto_other_pattern(186) -> 189;
yeccgoto_other_pattern(191) -> 167;
yeccgoto_other_pattern(199) -> 167;
yeccgoto_other_pattern(207) -> 167;
yeccgoto_other_pattern(209) -> 167;
yeccgoto_other_pattern(228) -> 167;
yeccgoto_other_pattern(231) -> 167;
yeccgoto_other_pattern(262) -> 167.

yeccgoto_primop_expr(108) -> 115;
yeccgoto_primop_expr(130) -> 115;
yeccgoto_primop_expr(131) -> 115;
yeccgoto_primop_expr(132) -> 115;
yeccgoto_primop_expr(133) -> 115;
yeccgoto_primop_expr(135) -> 115;
yeccgoto_primop_expr(136) -> 115;
yeccgoto_primop_expr(137) -> 115;
yeccgoto_primop_expr(138) -> 115;
yeccgoto_primop_expr(143) -> 115;
yeccgoto_primop_expr(144) -> 115;
yeccgoto_primop_expr(148) -> 115;
yeccgoto_primop_expr(159) -> 115;
yeccgoto_primop_expr(163) -> 115;
yeccgoto_primop_expr(182) -> 115;
yeccgoto_primop_expr(203) -> 115;
yeccgoto_primop_expr(240) -> 115;
yeccgoto_primop_expr(242) -> 115;
yeccgoto_primop_expr(246) -> 115;
yeccgoto_primop_expr(251) -> 115;
yeccgoto_primop_expr(254) -> 115;
yeccgoto_primop_expr(256) -> 115;
yeccgoto_primop_expr(258) -> 115;
yeccgoto_primop_expr(266) -> 115;
yeccgoto_primop_expr(273) -> 115;
yeccgoto_primop_expr(275) -> 115;
yeccgoto_primop_expr(293) -> 115;
yeccgoto_primop_expr(296) -> 115;
yeccgoto_primop_expr(307) -> 115.

yeccgoto_receive_expr(108) -> 114;
yeccgoto_receive_expr(130) -> 114;
yeccgoto_receive_expr(131) -> 114;
yeccgoto_receive_expr(132) -> 114;
yeccgoto_receive_expr(133) -> 114;
yeccgoto_receive_expr(135) -> 114;
yeccgoto_receive_expr(136) -> 114;
yeccgoto_receive_expr(137) -> 114;
yeccgoto_receive_expr(138) -> 114;
yeccgoto_receive_expr(143) -> 114;
yeccgoto_receive_expr(144) -> 114;
yeccgoto_receive_expr(148) -> 114;
yeccgoto_receive_expr(159) -> 114;
yeccgoto_receive_expr(163) -> 114;
yeccgoto_receive_expr(182) -> 114;
yeccgoto_receive_expr(203) -> 114;
yeccgoto_receive_expr(240) -> 114;
yeccgoto_receive_expr(242) -> 114;
yeccgoto_receive_expr(246) -> 114;
yeccgoto_receive_expr(251) -> 114;
yeccgoto_receive_expr(254) -> 114;
yeccgoto_receive_expr(256) -> 114;
yeccgoto_receive_expr(258) -> 114;
yeccgoto_receive_expr(266) -> 114;
yeccgoto_receive_expr(273) -> 114;
yeccgoto_receive_expr(275) -> 114;
yeccgoto_receive_expr(293) -> 114;
yeccgoto_receive_expr(296) -> 114;
yeccgoto_receive_expr(307) -> 114.

yeccgoto_segment(287) -> 289;
yeccgoto_segment(299) -> 289.

yeccgoto_segment_pattern(222) -> 224;
yeccgoto_segment_pattern(234) -> 224.

yeccgoto_segment_patterns(222) -> 223;
yeccgoto_segment_patterns(234) -> 235.

yeccgoto_segments(287) -> 288;
yeccgoto_segments(299) -> 300.

yeccgoto_sequence(108) -> 113;
yeccgoto_sequence(130) -> 113;
yeccgoto_sequence(131) -> 113;
yeccgoto_sequence(132) -> 113;
yeccgoto_sequence(133) -> 113;
yeccgoto_sequence(135) -> 113;
yeccgoto_sequence(136) -> 113;
yeccgoto_sequence(137) -> 113;
yeccgoto_sequence(138) -> 113;
yeccgoto_sequence(143) -> 113;
yeccgoto_sequence(144) -> 113;
yeccgoto_sequence(148) -> 113;
yeccgoto_sequence(159) -> 113;
yeccgoto_sequence(163) -> 113;
yeccgoto_sequence(182) -> 113;
yeccgoto_sequence(203) -> 113;
yeccgoto_sequence(240) -> 113;
yeccgoto_sequence(242) -> 113;
yeccgoto_sequence(246) -> 113;
yeccgoto_sequence(251) -> 113;
yeccgoto_sequence(254) -> 113;
yeccgoto_sequence(256) -> 113;
yeccgoto_sequence(258) -> 113;
yeccgoto_sequence(266) -> 113;
yeccgoto_sequence(273) -> 113;
yeccgoto_sequence(275) -> 113;
yeccgoto_sequence(293) -> 113;
yeccgoto_sequence(296) -> 113;
yeccgoto_sequence(307) -> 113.

yeccgoto_single_expression(108) -> 112;
yeccgoto_single_expression(130) -> 112;
yeccgoto_single_expression(131) -> 112;
yeccgoto_single_expression(132) -> 112;
yeccgoto_single_expression(133) -> 112;
yeccgoto_single_expression(135) -> 112;
yeccgoto_single_expression(136) -> 112;
yeccgoto_single_expression(137) -> 112;
yeccgoto_single_expression(138) -> 112;
yeccgoto_single_expression(143) -> 112;
yeccgoto_single_expression(144) -> 112;
yeccgoto_single_expression(148) -> 112;
yeccgoto_single_expression(159) -> 112;
yeccgoto_single_expression(163) -> 112;
yeccgoto_single_expression(182) -> 112;
yeccgoto_single_expression(203) -> 112;
yeccgoto_single_expression(240) -> 112;
yeccgoto_single_expression(242) -> 112;
yeccgoto_single_expression(246) -> 112;
yeccgoto_single_expression(251) -> 112;
yeccgoto_single_expression(254) -> 112;
yeccgoto_single_expression(256) -> 112;
yeccgoto_single_expression(258) -> 112;
yeccgoto_single_expression(266) -> 112;
yeccgoto_single_expression(273) -> 112;
yeccgoto_single_expression(275) -> 112;
yeccgoto_single_expression(293) -> 112;
yeccgoto_single_expression(296) -> 112;
yeccgoto_single_expression(307) -> 112.

yeccgoto_tail(271) -> 272;
yeccgoto_tail(278) -> 279.

yeccgoto_tail_constant(83) -> 84;
yeccgoto_tail_constant(90) -> 91.

yeccgoto_tail_literal(43) -> 45;
yeccgoto_tail_literal(51) -> 52.

yeccgoto_tail_pattern(205) -> 206;
yeccgoto_tail_pattern(212) -> 213.

yeccgoto_timeout(142) -> 166;
yeccgoto_timeout(176) -> 239.

yeccgoto_try_expr(108) -> 111;
yeccgoto_try_expr(130) -> 111;
yeccgoto_try_expr(131) -> 111;
yeccgoto_try_expr(132) -> 111;
yeccgoto_try_expr(133) -> 111;
yeccgoto_try_expr(135) -> 111;
yeccgoto_try_expr(136) -> 111;
yeccgoto_try_expr(137) -> 111;
yeccgoto_try_expr(138) -> 111;
yeccgoto_try_expr(143) -> 111;
yeccgoto_try_expr(144) -> 111;
yeccgoto_try_expr(148) -> 111;
yeccgoto_try_expr(159) -> 111;
yeccgoto_try_expr(163) -> 111;
yeccgoto_try_expr(182) -> 111;
yeccgoto_try_expr(203) -> 111;
yeccgoto_try_expr(240) -> 111;
yeccgoto_try_expr(242) -> 111;
yeccgoto_try_expr(246) -> 111;
yeccgoto_try_expr(251) -> 111;
yeccgoto_try_expr(254) -> 111;
yeccgoto_try_expr(256) -> 111;
yeccgoto_try_expr(258) -> 111;
yeccgoto_try_expr(266) -> 111;
yeccgoto_try_expr(273) -> 111;
yeccgoto_try_expr(275) -> 111;
yeccgoto_try_expr(293) -> 111;
yeccgoto_try_expr(296) -> 111;
yeccgoto_try_expr(307) -> 111.

yeccgoto_tuple(108) -> 110;
yeccgoto_tuple(130) -> 110;
yeccgoto_tuple(131) -> 110;
yeccgoto_tuple(132) -> 110;
yeccgoto_tuple(133) -> 110;
yeccgoto_tuple(135) -> 110;
yeccgoto_tuple(136) -> 110;
yeccgoto_tuple(137) -> 110;
yeccgoto_tuple(138) -> 110;
yeccgoto_tuple(143) -> 110;
yeccgoto_tuple(144) -> 110;
yeccgoto_tuple(148) -> 110;
yeccgoto_tuple(159) -> 110;
yeccgoto_tuple(163) -> 110;
yeccgoto_tuple(182) -> 110;
yeccgoto_tuple(203) -> 110;
yeccgoto_tuple(240) -> 110;
yeccgoto_tuple(242) -> 110;
yeccgoto_tuple(246) -> 110;
yeccgoto_tuple(251) -> 110;
yeccgoto_tuple(254) -> 110;
yeccgoto_tuple(256) -> 110;
yeccgoto_tuple(258) -> 110;
yeccgoto_tuple(266) -> 110;
yeccgoto_tuple(273) -> 110;
yeccgoto_tuple(275) -> 110;
yeccgoto_tuple(293) -> 110;
yeccgoto_tuple(296) -> 110;
yeccgoto_tuple(307) -> 110.

yeccgoto_tuple_constant(65) -> 66;
yeccgoto_tuple_constant(72) -> 66;
yeccgoto_tuple_constant(79) -> 66;
yeccgoto_tuple_constant(85) -> 66;
yeccgoto_tuple_constant(87) -> 66;
yeccgoto_tuple_constant(92) -> 66.

yeccgoto_tuple_literal(24) -> 25;
yeccgoto_tuple_literal(30) -> 25;
yeccgoto_tuple_literal(36) -> 25;
yeccgoto_tuple_literal(40) -> 25;
yeccgoto_tuple_literal(46) -> 25;
yeccgoto_tuple_literal(48) -> 25.

yeccgoto_tuple_pattern(142) -> 165;
yeccgoto_tuple_pattern(177) -> 165;
yeccgoto_tuple_pattern(179) -> 165;
yeccgoto_tuple_pattern(180) -> 165;
yeccgoto_tuple_pattern(181) -> 165;
yeccgoto_tuple_pattern(183) -> 165;
yeccgoto_tuple_pattern(186) -> 165;
yeccgoto_tuple_pattern(191) -> 165;
yeccgoto_tuple_pattern(199) -> 165;
yeccgoto_tuple_pattern(207) -> 165;
yeccgoto_tuple_pattern(209) -> 165;
yeccgoto_tuple_pattern(228) -> 165;
yeccgoto_tuple_pattern(231) -> 165;
yeccgoto_tuple_pattern(262) -> 165.

yeccgoto_variable(101) -> 102;
yeccgoto_variable(105) -> 303;
yeccgoto_variable(108) -> 109;
yeccgoto_variable(130) -> 109;
yeccgoto_variable(131) -> 109;
yeccgoto_variable(132) -> 109;
yeccgoto_variable(133) -> 109;
yeccgoto_variable(135) -> 109;
yeccgoto_variable(136) -> 109;
yeccgoto_variable(137) -> 109;
yeccgoto_variable(138) -> 109;
yeccgoto_variable(139) -> 102;
yeccgoto_variable(142) -> 102;
yeccgoto_variable(143) -> 109;
yeccgoto_variable(144) -> 109;
yeccgoto_variable(148) -> 109;
yeccgoto_variable(152) -> 102;
yeccgoto_variable(155) -> 102;
yeccgoto_variable(159) -> 109;
yeccgoto_variable(161) -> 102;
yeccgoto_variable(163) -> 109;
yeccgoto_variable(177) -> 102;
yeccgoto_variable(179) -> 188;
yeccgoto_variable(180) -> 102;
yeccgoto_variable(181) -> 102;
yeccgoto_variable(182) -> 109;
yeccgoto_variable(183) -> 102;
yeccgoto_variable(186) -> 188;
yeccgoto_variable(191) -> 102;
yeccgoto_variable(199) -> 102;
yeccgoto_variable(203) -> 109;
yeccgoto_variable(207) -> 102;
yeccgoto_variable(209) -> 102;
yeccgoto_variable(228) -> 102;
yeccgoto_variable(231) -> 102;
yeccgoto_variable(240) -> 109;
yeccgoto_variable(242) -> 109;
yeccgoto_variable(246) -> 109;
yeccgoto_variable(251) -> 109;
yeccgoto_variable(254) -> 109;
yeccgoto_variable(256) -> 109;
yeccgoto_variable(258) -> 109;
yeccgoto_variable(262) -> 102;
yeccgoto_variable(266) -> 109;
yeccgoto_variable(273) -> 109;
yeccgoto_variable(275) -> 109;
yeccgoto_variable(293) -> 109;
yeccgoto_variable(296) -> 109;
yeccgoto_variable(304) -> 102;
yeccgoto_variable(307) -> 109.

-compile({inline,{yeccpars2_9_,1}}).
-file("core_parse.yrl", 90).
yeccpars2_9_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_10_,1}}).
-file("core_parse.yrl", 86).
yeccpars2_10_([__2,__1 | Stack]) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_13_,1}}).
-file("core_parse.yrl", 289).
yeccpars2_13_([__3,__2,__1 | Stack]) ->
 [begin
   # c_fname { id = tok_val ( __1 ) , arity = tok_val ( __3 ) }
  end | Stack].

-compile({inline,{yeccpars2_15_,1}}).
-file("core_parse.yrl", 89).
yeccpars2_15_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_16_,1}}).
-file("core_parse.yrl", 87).
yeccpars2_16_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_17_,1}}).
-file("core_parse.yrl", 111).
yeccpars2_17_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_21_,1}}).
-file("core_parse.yrl", 98).
yeccpars2_21_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_22_,1}}).
-file("core_parse.yrl", 94).
yeccpars2_22_([__3,__2,__1 | Stack]) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_26_,1}}).
-file("core_parse.yrl", 253).
yeccpars2_26_([__1 | Stack]) ->
 [begin
   # c_literal { val = [ ] }
  end | Stack].

-compile({inline,{yeccpars2_27_,1}}).
-file("core_parse.yrl", 101).
yeccpars2_27_([__3,__2,__1 | Stack]) ->
 [begin
   { # c_literal { val = tok_val ( __1 ) } , __3 }
  end | Stack].

-compile({inline,{yeccpars2_31_,1}}).
-file("core_parse.yrl", 251).
yeccpars2_31_([__1 | Stack]) ->
 [begin
   # c_literal { val = tok_val ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_32_,1}}).
-file("core_parse.yrl", 248).
yeccpars2_32_([__1 | Stack]) ->
 [begin
   # c_literal { val = tok_val ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_33_,1}}).
-file("core_parse.yrl", 250).
yeccpars2_33_([__1 | Stack]) ->
 [begin
   # c_literal { val = tok_val ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_34_,1}}).
-file("core_parse.yrl", 249).
yeccpars2_34_([__1 | Stack]) ->
 [begin
   # c_literal { val = tok_val ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_35_,1}}).
-file("core_parse.yrl", 252).
yeccpars2_35_([__1 | Stack]) ->
 [begin
   # c_literal { val = tok_val ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_38_,1}}).
-file("core_parse.yrl", 246).
yeccpars2_38_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_39_,1}}).
-file("core_parse.yrl", 255).
yeccpars2_39_([__2,__1 | Stack]) ->
 [begin
   # c_tuple { es = [ ] }
  end | Stack].

-compile({inline,{yeccpars2_41_,1}}).
-file("core_parse.yrl", 245).
yeccpars2_41_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_42_,1}}).
-file("core_parse.yrl", 256).
yeccpars2_42_([__3,__2,__1 | Stack]) ->
 [begin
   # c_tuple { es = __2 }
  end | Stack].

-compile({inline,{yeccpars2_44_,1}}).
-file("core_parse.yrl", 67).
yeccpars2_44_([__2,__1 | Stack]) ->
 [begin
   { nil , tok_line ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_45_,1}}).
-file("core_parse.yrl", 258).
yeccpars2_45_([__3,__2,__1 | Stack]) ->
 [begin
   # c_cons { hd = __2 , tl = __3 }
  end | Stack].

-compile({inline,{yeccpars2_47_,1}}).
-file("core_parse.yrl", 260).
yeccpars2_47_([__1 | Stack]) ->
 [begin
   # c_literal { val = [ ] }
  end | Stack].

-compile({inline,{yeccpars2_50_,1}}).
-file("core_parse.yrl", 261).
yeccpars2_50_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_52_,1}}).
-file("core_parse.yrl", 262).
yeccpars2_52_([__3,__2,__1 | Stack]) ->
 [begin
   # c_cons { hd = __2 , tl = __3 }
  end | Stack].

-compile({inline,{yeccpars2_54_,1}}).
-file("core_parse.yrl", 97).
yeccpars2_54_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_55_,1}}).
-file("core_parse.yrl", 95).
yeccpars2_55_([__4,__3,__2,__1 | Stack]) ->
 [begin
   __3
  end | Stack].

-compile({inline,{yeccpars2_59_,1}}).
-file("core_parse.yrl", 111).
yeccpars2_59_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_67_,1}}).
-file("core_parse.yrl", 136).
yeccpars2_67_([__1 | Stack]) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_69_,1}}).
-file("core_parse.yrl", 129).
yeccpars2_69_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_73_,1}}).
-file("core_parse.yrl", 105).
yeccpars2_73_([__2,__1 | Stack]) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_74_,1}}).
-file("core_parse.yrl", 134).
yeccpars2_74_([__1 | Stack]) ->
 [begin
   tok_val ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_75_,1}}).
-file("core_parse.yrl", 131).
yeccpars2_75_([__1 | Stack]) ->
 [begin
   tok_val ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_76_,1}}).
-file("core_parse.yrl", 133).
yeccpars2_76_([__1 | Stack]) ->
 [begin
   tok_val ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_77_,1}}).
-file("core_parse.yrl", 132).
yeccpars2_77_([__1 | Stack]) ->
 [begin
   tok_val ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_78_,1}}).
-file("core_parse.yrl", 135).
yeccpars2_78_([__1 | Stack]) ->
 [begin
   tok_val ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_81_,1}}).
-file("core_parse.yrl", 138).
yeccpars2_81_([__2,__1 | Stack]) ->
 [begin
   { }
  end | Stack].

-compile({inline,{yeccpars2_82_,1}}).
-file("core_parse.yrl", 139).
yeccpars2_82_([__3,__2,__1 | Stack]) ->
 [begin
   list_to_tuple ( __2 )
  end | Stack].

-compile({inline,{yeccpars2_84_,1}}).
-file("core_parse.yrl", 141).
yeccpars2_84_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_86_,1}}).
-file("core_parse.yrl", 143).
yeccpars2_86_([__1 | Stack]) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_89_,1}}).
-file("core_parse.yrl", 144).
yeccpars2_89_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_91_,1}}).
-file("core_parse.yrl", 145).
yeccpars2_91_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_93_,1}}).
-file("core_parse.yrl", 128).
yeccpars2_93_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_94_,1}}).
-file("core_parse.yrl", 106).
yeccpars2_94_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_95_,1}}).
-file("core_parse.yrl", 293).
yeccpars2_95_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   core_lib : set_anno ( __2 , __4 )
  end | Stack].

-compile({inline,{yeccpars2_98_,1}}).
-file("core_parse.yrl", 115).
yeccpars2_98_([__3,__2,__1 | Stack]) ->
 [begin
   { __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_104_,1}}).
-file("core_parse.yrl", 202).
yeccpars2_104_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_107_,1}}).
-file("core_parse.yrl", 199).
yeccpars2_107_([__1 | Stack]) ->
 [begin
   # c_var { name = tok_val ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_128_,1}}).
-file("core_parse.yrl", 303).
yeccpars2_128_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # c_fun { vars = [ ] , body = __5 }
  end | Stack].

-compile({inline,{yeccpars2_134_,1}}).
-file("core_parse.yrl", 251).
yeccpars2_134_([__1 | Stack]) ->
 [begin
   # c_literal { val = tok_val ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_140_,1}}).
-file("core_parse.yrl", 111).
yeccpars2_140_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_146_,1}}).
-file("core_parse.yrl", 217).
yeccpars2_146_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_147_,1}}).
-file("core_parse.yrl", 264).
yeccpars2_147_([__2,__1 | Stack]) ->
 [begin
   # c_tuple { es = [ ] }
  end | Stack].

-compile({inline,{yeccpars2_149_,1}}).
-file("core_parse.yrl", 216).
yeccpars2_149_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_150_,1}}).
-file("core_parse.yrl", 265).
yeccpars2_150_([__3,__2,__1 | Stack]) ->
 [begin
   # c_tuple { es = __2 }
  end | Stack].

-compile({inline,{yeccpars2_154_,1}}).
-file("core_parse.yrl", 295).
yeccpars2_154_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_157_,1}}).
-file("core_parse.yrl", 296).
yeccpars2_157_([__2,__1 | Stack]) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_158_,1}}).
-file("core_parse.yrl", 297).
yeccpars2_158_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_164_,1}}).
-file("core_parse.yrl", 347).
yeccpars2_164_([__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   Len = length ( __8 ) ,
    if Len =:= 2 ; Len =:= 3 ->
    # c_try { arg = __2 , vars = __4 , body = __6 , evars = __8 , handler = __10 } ;
    true ->
    return_error ( tok_line ( __7 ) ,
    "expected 2 or 3 exception variables in 'try'" )
    end
  end | Stack].

-compile({inline,{yeccpars2_166_,1}}).
-file("core_parse.yrl", 358).
yeccpars2_166_([__2,__1 | Stack]) ->
 [begin
   { T , A } = __2 ,
    # c_receive { clauses = [ ] , timeout = T , action = A }
  end | Stack].

-compile({inline,{yeccpars2_175_,1}}).
-file("core_parse.yrl", 326).
yeccpars2_175_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_177_,1}}).
-file("core_parse.yrl", 317).
yeccpars2_177_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_185_,1}}).
-file("core_parse.yrl", 162).
yeccpars2_185_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_187_,1}}).
-file("core_parse.yrl", 173).
yeccpars2_187_([__2,__1 | Stack]) ->
 [begin
   # c_tuple { es = [ ] }
  end | Stack].

-compile({inline,{yeccpars2_192_,1}}).
-file("core_parse.yrl", 169).
yeccpars2_192_([__3,__2,__1 | Stack]) ->
 [begin
   # c_alias { var = __1 , pat = __3 }
  end | Stack].

-compile({inline,{yeccpars2_195_,1}}).
-file("core_parse.yrl", 157).
yeccpars2_195_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   core_lib : set_anno ( __2 , __4 )
  end | Stack].

-compile({inline,{yeccpars2_198_,1}}).
-file("core_parse.yrl", 206).
yeccpars2_198_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   core_lib : set_anno ( __2 , __4 )
  end | Stack].

-compile({inline,{yeccpars2_200_,1}}).
-file("core_parse.yrl", 161).
yeccpars2_200_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_201_,1}}).
-file("core_parse.yrl", 174).
yeccpars2_201_([__3,__2,__1 | Stack]) ->
 [begin
   # c_tuple { es = __2 }
  end | Stack].

-compile({inline,{yeccpars2_204_,1}}).
-file("core_parse.yrl", 365).
yeccpars2_204_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { __2 , __4 }
  end | Stack].

-compile({inline,{yeccpars2_206_,1}}).
-file("core_parse.yrl", 177).
yeccpars2_206_([__3,__2,__1 | Stack]) ->
 [begin
   # c_cons { hd = __2 , tl = __3 }
  end | Stack].

-compile({inline,{yeccpars2_208_,1}}).
-file("core_parse.yrl", 179).
yeccpars2_208_([__1 | Stack]) ->
 [begin
   # c_literal { val = [ ] }
  end | Stack].

-compile({inline,{yeccpars2_211_,1}}).
-file("core_parse.yrl", 180).
yeccpars2_211_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_213_,1}}).
-file("core_parse.yrl", 182).
yeccpars2_213_([__3,__2,__1 | Stack]) ->
 [begin
   # c_cons { hd = __2 , tl = __3 }
  end | Stack].

-compile({inline,{yeccpars2_215_,1}}).
-file("core_parse.yrl", 327).
yeccpars2_215_([__2,__1 | Stack]) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_216_,1}}).
-file("core_parse.yrl", 328).
yeccpars2_216_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_221_,1}}).
-file("core_parse.yrl", 321).
yeccpars2_221_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   core_lib : set_anno ( __2 , __4 )
  end | Stack].

-compile({inline,{yeccpars2_224_,1}}).
-file("core_parse.yrl", 188).
yeccpars2_224_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_227_,1}}).
-file("core_parse.yrl", 184).
yeccpars2_227_([__4,__3,__2,__1 | Stack]) ->
 [begin
   # c_binary { segments = [ ] }
  end | Stack].

-compile({inline,{yeccpars2_233_,1}}).
-file("core_parse.yrl", 191).
yeccpars2_233_([__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   case __6 of
    [ S , U , T , Fs ] ->
    # c_bitstr { val = __3 , size = S , unit = U , type = T , flags = Fs } ;
    true ->
    return_error ( tok_line ( __1 ) ,
    "expected 4 arguments in binary segment" )
    end
  end | Stack].

-compile({inline,{yeccpars2_235_,1}}).
-file("core_parse.yrl", 187).
yeccpars2_235_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_237_,1}}).
-file("core_parse.yrl", 185).
yeccpars2_237_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # c_binary { segments = __3 }
  end | Stack].

-compile({inline,{yeccpars2_238_,1}}).
-file("core_parse.yrl", 316).
yeccpars2_238_([__2,__1 | Stack]) ->
 [begin
   [ __1 | __2 ]
  end | Stack].

-compile({inline,{yeccpars2_239_,1}}).
-file("core_parse.yrl", 361).
yeccpars2_239_([__3,__2,__1 | Stack]) ->
 [begin
   { T , A } = __3 ,
    # c_receive { clauses = __2 , timeout = T , action = A }
  end | Stack].

-compile({inline,{yeccpars2_243_,1}}).
-file("core_parse.yrl", 324).
yeccpars2_243_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # c_clause { pats = __1 , guard = __3 , body = __5 }
  end | Stack].

-compile({inline,{yeccpars2_245_,1}}).
-file("core_parse.yrl", 338).
yeccpars2_245_([__3,__2,__1 | Stack]) ->
 [begin
   Name = # c_literal { val = tok_val ( __2 ) } ,
    # c_primop { name = Name , args = __3 }
  end | Stack].

-compile({inline,{yeccpars2_248_,1}}).
-file("core_parse.yrl", 341).
yeccpars2_248_([__2,__1 | Stack]) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_249_,1}}).
-file("core_parse.yrl", 342).
yeccpars2_249_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_252_,1}}).
-file("core_parse.yrl", 311).
yeccpars2_252_([__4,__3,__2,__1 | Stack]) ->
 [begin
   # c_letrec { defs = __2 , body = __4 }
  end | Stack].

-compile({inline,{yeccpars2_257_,1}}).
-file("core_parse.yrl", 308).
yeccpars2_257_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # c_let { vars = __2 , arg = __4 , body = __6 }
  end | Stack].

-compile({inline,{yeccpars2_259_,1}}).
-file("core_parse.yrl", 300).
yeccpars2_259_([__3,__2,__1 | Stack]) ->
 [begin
   # c_seq { arg = __2 , body = __3 }
  end | Stack].

-compile({inline,{yeccpars2_260_,1}}).
-file("core_parse.yrl", 355).
yeccpars2_260_([__2,__1 | Stack]) ->
 [begin
   # c_catch { body = __2 }
  end | Stack].

-compile({inline,{yeccpars2_264_,1}}).
-file("core_parse.yrl", 314).
yeccpars2_264_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # c_case { arg = __2 , clauses = __4 }
  end | Stack].

-compile({inline,{yeccpars2_268_,1}}).
-file("core_parse.yrl", 335).
yeccpars2_268_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # c_call { module = __2 , name = __4 , args = __5 }
  end | Stack].

-compile({inline,{yeccpars2_270_,1}}).
-file("core_parse.yrl", 331).
yeccpars2_270_([__3,__2,__1 | Stack]) ->
 [begin
   # c_apply { op = __2 , args = __3 }
  end | Stack].

-compile({inline,{yeccpars2_272_,1}}).
-file("core_parse.yrl", 267).
yeccpars2_272_([__3,__2,__1 | Stack]) ->
 [begin
   # c_cons { hd = __2 , tl = __3 }
  end | Stack].

-compile({inline,{yeccpars2_274_,1}}).
-file("core_parse.yrl", 269).
yeccpars2_274_([__1 | Stack]) ->
 [begin
   # c_literal { val = [ ] }
  end | Stack].

-compile({inline,{yeccpars2_277_,1}}).
-file("core_parse.yrl", 270).
yeccpars2_277_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_279_,1}}).
-file("core_parse.yrl", 271).
yeccpars2_279_([__3,__2,__1 | Stack]) ->
 [begin
   # c_cons { hd = __2 , tl = __3 }
  end | Stack].

-compile({inline,{yeccpars2_281_,1}}).
-file("core_parse.yrl", 219).
yeccpars2_281_([__2,__1 | Stack]) ->
 [begin
   # c_values { es = [ ] }
  end | Stack].

-compile({inline,{yeccpars2_282_,1}}).
-file("core_parse.yrl", 220).
yeccpars2_282_([__3,__2,__1 | Stack]) ->
 [begin
   # c_values { es = __2 }
  end | Stack].

-compile({inline,{yeccpars2_286_,1}}).
-file("core_parse.yrl", 214).
yeccpars2_286_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   core_lib : set_anno ( __2 , __4 )
  end | Stack].

-compile({inline,{yeccpars2_289_,1}}).
-file("core_parse.yrl", 277).
yeccpars2_289_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_292_,1}}).
-file("core_parse.yrl", 273).
yeccpars2_292_([__4,__3,__2,__1 | Stack]) ->
 [begin
   # c_literal { val = << >> }
  end | Stack].

-compile({inline,{yeccpars2_298_,1}}).
-file("core_parse.yrl", 280).
yeccpars2_298_([__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   case __6 of
    [ S , U , T , Fs ] ->
    # c_bitstr { val = __3 , size = S , unit = U , type = T , flags = Fs } ;
    true ->
    return_error ( tok_line ( __1 ) ,
    "expected 4 arguments in binary segment" )
    end
  end | Stack].

-compile({inline,{yeccpars2_300_,1}}).
-file("core_parse.yrl", 276).
yeccpars2_300_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_302_,1}}).
-file("core_parse.yrl", 274).
yeccpars2_302_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # c_binary { segments = __3 }
  end | Stack].

-compile({inline,{yeccpars2_305_,1}}).
-file("core_parse.yrl", 201).
yeccpars2_305_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_308_,1}}).
-file("core_parse.yrl", 305).
yeccpars2_308_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # c_fun { vars = __3 , body = __6 }
  end | Stack].

-compile({inline,{yeccpars2_312_,1}}).
-file("core_parse.yrl", 118).
yeccpars2_312_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   core_lib : set_anno ( __2 , __4 )
  end | Stack].

-compile({inline,{yeccpars2_313_,1}}).
-file("core_parse.yrl", 109).
yeccpars2_313_([__2,__1 | Stack]) ->
 [begin
   [ __1 | __2 ]
  end | Stack].

-compile({inline,{yeccpars2_314_,1}}).
-file("core_parse.yrl", 78).
yeccpars2_314_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # c_module { name = # c_literal { val = tok_val ( __2 ) } , exports = __3 ,
    attrs = __4 , defs = __5 }
  end | Stack].

-compile({inline,{yeccpars2_318_,1}}).
-file("core_parse.yrl", 111).
yeccpars2_318_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_323_,1}}).
-file("core_parse.yrl", 83).
yeccpars2_323_([__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   # c_module { anno = __9 , name = tok_val ( __3 ) , exports = __4 ,
    attrs = __5 , defs = __6 }
  end | Stack].


-file("core_parse.yrl", 391).
