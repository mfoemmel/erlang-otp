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
build_type({atom,_,maybe_improper_list}, [C,T]) -> 
    {type, maybe_improper_list, [C,T]};
build_type({atom,_,maybe_improper_list}, []) -> 
    {type, maybe_improper_list, []};
build_type({atom,_,ref}, []) -> {type, ref, []};
build_type({atom,_,string}, []) -> {type, string, []};
build_type({atom,_,tuple}, []) -> {type, tuple, []};
build_type({atom,La,_}, _) -> error_bad_decl(La,type).

lift_unions(T1, {type, union, List}) ->
    {type, union, [T1|List]};
lift_unions(T1, T2 = {type, _, _}) ->
    {type, union, [T1, T2]}.

get_atom({atom, _, Atom}) when is_atom(Atom) -> Atom.

get_int({integer, _, Integer}) when is_integer(Integer) -> Integer.

error_bad_decl(L, S) ->
    return_error(L, io_lib:format("bad ~w declaration", [S])).

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



-file("./typer_parse.erl", 194).

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
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
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
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.2",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 5, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr).

yeccpars2_1(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_top_type(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_2(_S, dot, _Ss, Stack,  _T, _Ts, _Tzr) ->
 {ok, hd(Stack)}.

yeccpars2_3(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr).

yeccpars2_4(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 34, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_5: see yeccpars2_0

yeccpars2_6(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 5, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr).

yeccpars2_7(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_7_(Stack),
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_8_(Stack),
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_9(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 5, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 12, Ss, Stack, T, Ts, Tzr).

yeccpars2_10(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 15, Ss, Stack, T, Ts, Tzr).

yeccpars2_11(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 13, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_11_(Stack),
 yeccpars2(yeccgoto_top_types(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_12_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_13: see yeccpars2_0

yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_14_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_top_types(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_15_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_16(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 5, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr).

yeccpars2_17(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr).

yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_18_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_19_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_20: see yeccpars2_0

yeccpars2_21(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr).

yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_22_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_23(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_23(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr).

yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_24_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_25(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr).

yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_26_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_27(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr).

yeccpars2_28(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr).

yeccpars2_29(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr).

yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_30_(Stack),
 Nss = lists:nthtail(6, Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_31(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 32, Ss, Stack, T, Ts, Tzr).

yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_32_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_33(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 3, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 4, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 5, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 9, Ss, Stack, T, Ts, Tzr).

yeccpars2_34(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 35, Ss, Stack, T, Ts, Tzr).

yeccpars2_35(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr).

yeccpars2_36(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 37, Ss, Stack, T, Ts, Tzr).

yeccpars2_37(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr).

yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_38_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_39(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 44, Ss, Stack, T, Ts, Tzr).

yeccpars2_40(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_41: see yeccpars2_0

yeccpars2_42(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 43, Ss, Stack, T, Ts, Tzr).

yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_43_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_44(S, '->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 45, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_45: see yeccpars2_0

yeccpars2_46(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr).

yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_47_(Stack),
 Nss = lists:nthtail(6, Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_48(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr).

yeccpars2_49(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr).

yeccpars2_50(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr).

yeccpars2_51(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_51_(Stack),
 yeccpars2(yeccgoto_record_fields(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_52(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr).

yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_53_(Stack),
 Nss = lists:nthtail(3, Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_54: see yeccpars2_0

yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_55_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_record_field(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_56(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr).

yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_57_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_record_fields(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_58_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_59: see yeccpars2_0

yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_60_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_top_type(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccgoto_record_field(49) -> 51;
yeccgoto_record_field(56) -> 51.

yeccgoto_record_fields(49) -> 50;
yeccgoto_record_fields(56) -> 57.

yeccgoto_top_type(0) -> 2;
yeccgoto_top_type(5) -> 11;
yeccgoto_top_type(6) -> 23;
yeccgoto_top_type(9) -> 11;
yeccgoto_top_type(13) -> 11;
yeccgoto_top_type(16) -> 17;
yeccgoto_top_type(20) -> 21;
yeccgoto_top_type(33) -> 11;
yeccgoto_top_type(41) -> 42;
yeccgoto_top_type(45) -> 46;
yeccgoto_top_type(54) -> 55;
yeccgoto_top_type(59) -> 60.

yeccgoto_top_types(5) -> 31;
yeccgoto_top_types(9) -> 10;
yeccgoto_top_types(13) -> 14;
yeccgoto_top_types(33) -> 39.

yeccgoto_type(0) -> 1;
yeccgoto_type(5) -> 1;
yeccgoto_type(6) -> 1;
yeccgoto_type(9) -> 1;
yeccgoto_type(13) -> 1;
yeccgoto_type(16) -> 1;
yeccgoto_type(20) -> 1;
yeccgoto_type(33) -> 1;
yeccgoto_type(41) -> 1;
yeccgoto_type(45) -> 1;
yeccgoto_type(54) -> 1;
yeccgoto_type(59) -> 1.

-compile({inline,{yeccpars2_7_,1}}).
-file("typer_parse.yrl", 17).
yeccpars2_7_([__1 | Stack]) ->
 [begin
   { type , atom , [ get_atom ( __1 ) ] }
  end | Stack].

-compile({inline,{yeccpars2_8_,1}}).
-file("typer_parse.yrl", 30).
yeccpars2_8_([__1 | Stack]) ->
 [begin
   { type , integer , [ get_int ( __1 ) ] }
  end | Stack].

-compile({inline,{yeccpars2_11_,1}}).
-file("typer_parse.yrl", 11).
yeccpars2_11_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_12_,1}}).
-file("typer_parse.yrl", 26).
yeccpars2_12_([__2,__1 | Stack]) ->
 [begin
   { type , tuple , [ ] }
  end | Stack].

-compile({inline,{yeccpars2_14_,1}}).
-file("typer_parse.yrl", 12).
yeccpars2_14_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_15_,1}}).
-file("typer_parse.yrl", 27).
yeccpars2_15_([__3,__2,__1 | Stack]) ->
 [begin
   { type , tuple , __2 }
  end | Stack].

-compile({inline,{yeccpars2_18_,1}}).
-file("typer_parse.yrl", 18).
yeccpars2_18_([__3,__2,__1 | Stack]) ->
 [begin
   build_type ( __1 , [ ] )
  end | Stack].

-compile({inline,{yeccpars2_19_,1}}).
-file("typer_parse.yrl", 19).
yeccpars2_19_([__4,__3,__2,__1 | Stack]) ->
 [begin
   build_type ( __1 , [ __3 ] )
  end | Stack].

-compile({inline,{yeccpars2_22_,1}}).
-file("typer_parse.yrl", 20).
yeccpars2_22_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   build_type ( __1 , [ __3 , __5 ] )
  end | Stack].

-compile({inline,{yeccpars2_24_,1}}).
-file("typer_parse.yrl", 21).
yeccpars2_24_([__2,__1 | Stack]) ->
 [begin
   { type , nil , [ ] }
  end | Stack].

-compile({inline,{yeccpars2_26_,1}}).
-file("typer_parse.yrl", 22).
yeccpars2_26_([__3,__2,__1 | Stack]) ->
 [begin
   { type , list , [ __2 ] }
  end | Stack].

-compile({inline,{yeccpars2_30_,1}}).
-file("typer_parse.yrl", 23).
yeccpars2_30_([__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { type , nonempty_list , [ __2 ] }
  end | Stack].

-compile({inline,{yeccpars2_32_,1}}).
-file("typer_parse.yrl", 31).
yeccpars2_32_([__3,__2,__1 | Stack]) ->
 [begin
   { type , product , __2 }
  end | Stack].

-compile({inline,{yeccpars2_38_,1}}).
-file("typer_parse.yrl", 33).
yeccpars2_38_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { type , range , [ get_int ( __2 ) , get_int ( __5 ) ] }
  end | Stack].

-compile({inline,{yeccpars2_43_,1}}).
-file("typer_parse.yrl", 24).
yeccpars2_43_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { type , 'fun' , [ [ ] , __5 ] }
  end | Stack].

-compile({inline,{yeccpars2_47_,1}}).
-file("typer_parse.yrl", 25).
yeccpars2_47_([__7,__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { type , 'fun' , [ __3 , __6 ] }
  end | Stack].

-compile({inline,{yeccpars2_51_,1}}).
-file("typer_parse.yrl", 35).
yeccpars2_51_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_53_,1}}).
-file("typer_parse.yrl", 28).
yeccpars2_53_([__4,__3,__2,__1 | Stack]) ->
 [begin
   { type , record , [ get_atom ( __2 ) ] }
  end | Stack].

-compile({inline,{yeccpars2_55_,1}}).
-file("typer_parse.yrl", 38).
yeccpars2_55_([__3,__2,__1 | Stack]) ->
 [begin
   { get_atom ( __1 ) , __3 }
  end | Stack].

-compile({inline,{yeccpars2_57_,1}}).
-file("typer_parse.yrl", 36).
yeccpars2_57_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_58_,1}}).
-file("typer_parse.yrl", 29).
yeccpars2_58_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   { type , record , [ get_atom ( __2 ) | __4 ] }
  end | Stack].

-compile({inline,{yeccpars2_60_,1}}).
-file("typer_parse.yrl", 15).
yeccpars2_60_([__3,__2,__1 | Stack]) ->
 [begin
   lift_unions ( __1 , __3 )
  end | Stack].


-file("typer_parse.yrl", 89).
