-module(xref_parser).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("xref_parser.yrl", 107).

-export([t2s/1]).

-import(lists, [concat/1, flatten/1]).

%%% Syntax of the parse tree:
%%% Start = [Statement]
%%% Statement = {assign, AOp, VarName, Expr}
%%%           | Expr
%%% AOp = tmp | user
%%% Expr = Constants | Variable | Unary | Binary | RegExpr
%%% Constants = {list, [Constant]}  % not empty list
%%%           | {tuple, [Constant]}
%%%           | Constant % only to avoid [ and ] in error messages...
%%% Constant = {constant, 'Fun', vertex, MFA} | 
%%%            {constant, AtomType, vertex, atom()} |
%%%            {constant, 'Fun', edge, {MFA, MFA}} | 
%%%            {constant, AtomType, edge, {atom(), atom()}}
%%% Variable = {variable, VarName}
%%% VarName = atom()
%%% Unary = {set, SetUOp, Expr} 
%%%       | {graph, GraphUOp, Expr}
%%%       | {type, {TypeOp, Type}, Expr}
%%%       | {numeric, NumOp, Expr, Expr}
%%% SetUOp = range | domain | weak | strict
%%% GraphUOp = components | condensation | closure
%%% Binary = {set, SetBOp, Expr, Expr}
%%%        | {restr, RestrOp, Expr, Expr}
%%%        | {path, Expr, Expr}
%%% SetBOp = union | intersection | difference
%%% RestrOp = '|' | '||' | '|||'
%%% TypeOp = type | convert
%%% NumOp = '#'
%%% RegExpr = {regexpr, RExpr, Type}
%%% RExpr = string() | {AtomReg, AtomReg, IntReg}
%%% AtomReg = string() | atom() | variable()
%%% IntReg = string() | integer()
%%% MFA = {atom(), atom(), integer()}
%%% Type = 'Rel' | 'App' | 'Mod' | 'Fun'
%%%      | 'Lin' | 'LLin' | 'XLin' | 'ELin' | 'XXL'
%%% AtomType = unknown | 'Rel' | 'App' | 'Mod'

value_of(Token) ->
    element(3, Token).

prefix(Op, Expr) ->
    case is_prefix_op(Op) of
	false ->
	    return_error(0, ["invalid_operator", Op]);
	UOp ->
	    {UOp, Op, Expr}
    end.

is_prefix_op(range) -> set;
is_prefix_op(domain) -> set;
is_prefix_op(weak) -> set;
is_prefix_op(strict) -> set;
is_prefix_op(components) -> graph;
is_prefix_op(condensation) -> graph;
is_prefix_op(closure) -> graph;
is_prefix_op('#') -> numeric;
is_prefix_op(_) -> false.

check_regexp(String) ->
    case regexp:parse(String) of
	{ok, _Expr} ->
	    {regexpr, String};
	{error, Reason} ->
	    F = regexp:format_error(Reason),
	    return_error(0, ["invalid_regexp", String, F])
    end.

check_regexp_variable('_') ->
    variable;
check_regexp_variable(Var) ->
    return_error(0, ["invalid_regexp_variable", Var]).

regexp(func, RExpr, unknown) ->
    {regexpr, RExpr, 'Fun'};
regexp(_, RExpr, unknown) ->
    return_error(0, ["missing_type", t2s({regexpr, RExpr, unknown})]);
regexp(Kind, RExpr, Type) ->
    E = {type, {type, Type}, {regexpr, RExpr, Type}},
    case Type of
	'Fun' when Kind =:= func -> E;
	'Mod' when Kind =:= atom -> E;
	'App' when Kind =:= atom -> E;
	'Rel' when Kind =:= atom -> E;
	_Else -> return_error(0, ["type_mismatch", t2s(E)])
    end.

type(Expr, unknown) ->
    Expr;
type(Expr, Type) ->
    {type, {type, Type}, type_constants(Expr, Type, Expr)}.

type_constants({list, L}, Type, E) ->
    {list, type_constants(L, Type, E)};
type_constants({tuple, L}, Type, E) ->
    {tuple, type_constants(L, Type, E)};
type_constants([C | Cs], Type, E) ->
    [type_constants(C, Type, E) | type_constants(Cs, Type, E)];
type_constants([], _Type, _E) ->
    [];
type_constants({constant, unknown, OType, Con}, 'Rel', _E) ->
    {constant, 'Rel', OType, Con};
type_constants({constant, unknown, OType, Con}, 'App', _E) ->
    {constant, 'App', OType, Con};
type_constants({constant, unknown, OType, Con}, 'Mod', _E) ->
    {constant, 'Mod', OType, Con};
type_constants(C={constant, Type, _OType, _Con}, Type, _E) ->
    C;
type_constants(_C, Type, E) ->
    return_error(0, ["type_mismatch", t2s({type, {type, Type}, E})]).

t2s(T) ->
    concat(flatten(e2s(T, 0))).

%% Does not handle list of statements.
e2s({assign, VarType, Name, E}, P) ->
    [left(P, 100), Name, name_it(VarType), e2s(E, 100), right(P, 100)];
e2s({constant, 'Fun', vertex, MFA}, _P) ->
    mfa2s(MFA);
e2s({constant, _Type, vertex, A}, _P) ->
    [c2s(A)];
e2s({constant, 'Fun', edge, {MFA1,MFA2}}, _P) ->
    [mfa2s(MFA1),' -> ',mfa2s(MFA2)];
e2s({constant, _Type, edge, {A1,A2}}, _P) ->
    [c2s(A1),' -> ',c2s(A2)];
e2s({variable, Name}, _P) ->
    [Name];
e2s({list, E}, _P) ->
    ['[', e2s(E, 0), ']'];
e2s({tuple, E}, _P) ->
    ['{', e2s(E, 0), '}'];
e2s({type, {convert, Type}, E}, P) ->
    [left(P, 700), '(',Type,') ', e2s(E, 700), right(P, 700)];
e2s({type, {type, Type}, E}, P) ->
    [left(P, 700), e2s(E, 700), ' : ', Type, right(P, 700)];
e2s({set, Op, E}, P) ->
    [left(P, 700), name_it(Op), ' ', e2s(E, 700), right(P, 700)];
e2s({graph, Op, E}, P) ->
    [left(P, 700), name_it(Op), ' ', e2s(E, 700), right(P, 700)];
e2s({numeric, Op, E}, P) ->
    [left(P, 400), name_it(Op), ' ', e2s(E, 400), right(P, 400)];
e2s({set, Op, E1, E2}, P) ->
    P1 = prio(Op),
    [left(P, P1), e2s(E1, P1),name_it(Op),e2s(E2, P1+50), right(P, P1)];
e2s({path, E1, E2}, P) ->
    P1 = 600,
    [left(P, P1), e2s(E1, P1),' of ',e2s(E2, P1+50), right(P, P1)];
e2s({regexpr, Expr={regexpr,_}, _Type}, _P) ->
    [re(Expr)];
e2s({regexpr, {M,F,A}, _Type}, _P) ->
    [re(M),':',re(F),'/', re(A)];
e2s({restr, Op, E1, E2}, P) ->
    P1 = 500,
    [left(P, P1), e2s(E1, P1),name_it(Op),e2s(E2, P1+50), right(P, P1)];
e2s([], _P) ->
    [];
e2s([E], P) ->
    e2s(E, P);
e2s([E | Es], P) ->
    [e2s(E, P),', ',e2s(Es, P)].

mfa2s({M,F,A}) ->
    [c2s(M),':',c2s(F),'/',A].

c2s(C) ->
    [S] = io_lib:format("~p", [C]),
    list_to_atom(S).

re(variable) -> ['_'];
re({atom, Atom}) -> [Atom];
re({integer, Int}) -> [Int];
re({regexpr, Str}) -> ['"',erlang:list_to_atom(Str),'"'].

left(P1, P2) when P1 > P2 -> ['('];
left(_P1, _P2) -> [].

right(P1, P2) when P1 > P2 -> [')'];
right(_P1, _P2) -> [].

prio(intersection) -> 300;
prio(difference)   -> 200;
prio(union)        -> 200.

name_it(tmp)           -> ' = ';
name_it(user)          -> ' := ';
name_it('|')           -> ' | ';
name_it('||')          -> ' || ';
name_it('|||')         -> ' ||| ';
name_it(union)         -> ' + ';
name_it(intersection)  -> ' * ';
name_it(difference)    -> ' - ';
name_it(Name) -> Name.   

-file("/ldisk/daily_build/otp_prebuild_r12b.2008-02-05_20/otp_src_R12B-1/bootstrap/lib/parsetools/include/yeccpre.hrl", 0).
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



-file("./xref_parser.erl", 345).

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
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
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
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.2",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, edge, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, vertex, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr).

yeccpars2_1(_S, '$end', _Ss, Stack,  _T, _Ts, _Tzr) ->
 {ok, hd(Stack)}.

yeccpars2_2(S, ':=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_2_$end'(Stack),
 yeccpars2(yeccgoto_expr(hd(Ss)), '$end', Ss, NewStack, T, Ts, Tzr);
yeccpars2_2(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_2_*'(Stack),
 yeccpars2(yeccgoto_expr(hd(Ss)), '*', Ss, NewStack, T, Ts, Tzr);
yeccpars2_2(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_2_+'(Stack),
 yeccpars2(yeccgoto_expr(hd(Ss)), '+', Ss, NewStack, T, Ts, Tzr);
yeccpars2_2(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_2_-'(Stack),
 yeccpars2(yeccgoto_expr(hd(Ss)), '-', Ss, NewStack, T, Ts, Tzr);
yeccpars2_2(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_2_of(Stack),
 yeccpars2(yeccgoto_expr(hd(Ss)), 'of', Ss, NewStack, T, Ts, Tzr);
yeccpars2_2(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_2_|'(Stack),
 yeccpars2(yeccgoto_expr(hd(Ss)), '|', Ss, NewStack, T, Ts, Tzr);
yeccpars2_2(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_2_||'(Stack),
 yeccpars2(yeccgoto_expr(hd(Ss)), '||', Ss, NewStack, T, Ts, Tzr);
yeccpars2_2(_S, '|||', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_2_|||'(Stack),
 yeccpars2(yeccgoto_expr(hd(Ss)), '|||', Ss, NewStack, T, Ts, Tzr);
yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_2_(Stack),
 yeccpars2(yeccgoto_regvar(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_xref(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_4(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_4_(Stack),
 yeccpars2(yeccgoto_statements(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_regatom(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_6(S, decl, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_6_$end'(Stack),
 yeccpars2(73, '$end', [6 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_6(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_6_)'(Stack),
 yeccpars2(73, ')', [6 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_6(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_6_*'(Stack),
 yeccpars2(73, '*', [6 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_6(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_6_+'(Stack),
 yeccpars2(73, '+', [6 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_6(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_6_,'(Stack),
 yeccpars2(73, ',', [6 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_6(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_6_-'(Stack),
 yeccpars2(73, '-', [6 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_6(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_6_of(Stack),
 yeccpars2(73, 'of', [6 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_6(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_6_|'(Stack),
 yeccpars2(73, '|', [6 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_6(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_6_||'(Stack),
 yeccpars2(73, '||', [6 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_6(_S, '|||', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_6_|||'(Stack),
 yeccpars2(73, '|||', [6 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_regatom(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_expr(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_8(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_9: see yeccpars2_0

yeccpars2_10(_S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_prefix_op(hd(Ss)), '#', Ss, Stack, T, Ts, Tzr);
yeccpars2_10(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_prefix_op(hd(Ss)), '(', Ss, Stack, T, Ts, Tzr);
yeccpars2_10(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_prefix_op(hd(Ss)), '[', Ss, Stack, T, Ts, Tzr);
yeccpars2_10(_S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_prefix_op(hd(Ss)), atom, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(_S, edge, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_prefix_op(hd(Ss)), edge, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(_S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_prefix_op(hd(Ss)), string, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(_S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_prefix_op(hd(Ss)), var, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(_S, vertex, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_prefix_op(hd(Ss)), vertex, Ss, Stack, T, Ts, Tzr);
yeccpars2_10(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_prefix_op(hd(Ss)), '{', Ss, Stack, T, Ts, Tzr);
yeccpars2_10(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_10_$end'(Stack),
 yeccpars2(yeccgoto_const(hd(Ss)), '$end', Ss, NewStack, T, Ts, Tzr);
yeccpars2_10(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_10_)'(Stack),
 yeccpars2(yeccgoto_const(hd(Ss)), ')', Ss, NewStack, T, Ts, Tzr);
yeccpars2_10(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_10_*'(Stack),
 yeccpars2(yeccgoto_const(hd(Ss)), '*', Ss, NewStack, T, Ts, Tzr);
yeccpars2_10(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_10_+'(Stack),
 yeccpars2(yeccgoto_const(hd(Ss)), '+', Ss, NewStack, T, Ts, Tzr);
yeccpars2_10(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_10_,'(Stack),
 yeccpars2(yeccgoto_const(hd(Ss)), ',', Ss, NewStack, T, Ts, Tzr);
yeccpars2_10(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_10_-'(Stack),
 yeccpars2(yeccgoto_const(hd(Ss)), '-', Ss, NewStack, T, Ts, Tzr);
yeccpars2_10(_S, decl, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_decl(Stack),
 yeccpars2(yeccgoto_const(hd(Ss)), decl, Ss, NewStack, T, Ts, Tzr);
yeccpars2_10(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_of(Stack),
 yeccpars2(yeccgoto_const(hd(Ss)), 'of', Ss, NewStack, T, Ts, Tzr);
yeccpars2_10(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_10_|'(Stack),
 yeccpars2(yeccgoto_const(hd(Ss)), '|', Ss, NewStack, T, Ts, Tzr);
yeccpars2_10(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_10_||'(Stack),
 yeccpars2(yeccgoto_const(hd(Ss)), '||', Ss, NewStack, T, Ts, Tzr);
yeccpars2_10(_S, '|||', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_10_|||'(Stack),
 yeccpars2(yeccgoto_const(hd(Ss)), '|||', Ss, NewStack, T, Ts, Tzr);
yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_10_(Stack),
 yeccpars2(yeccgoto_regatom(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_11(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, 'of', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(S, '|||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_11_(Stack),
 yeccpars2(yeccgoto_statements(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_12: see yeccpars2_0

yeccpars2_13(S, decl, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_13_(Stack),
 yeccpars2(59, Cat, [13 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_constant(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_15: see yeccpars2_0

yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_16_(Stack),
 yeccpars2(yeccgoto_count_op(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_17(S, '#', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, cast, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, edge, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, vertex, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr).

yeccpars2_18(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(S, edge, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_18(S, vertex, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr).

yeccpars2_19(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_19_(Stack),
 yeccpars2(yeccgoto_id(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_20_(Stack),
 yeccpars2(yeccgoto_const(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_21_(Stack),
 yeccpars2(yeccgoto_regstr(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_22_(Stack),
 yeccpars2(yeccgoto_variable(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_23_(Stack),
 yeccpars2(yeccgoto_const(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_24: see yeccpars2_18

yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_25_(Stack),
 yeccpars2(yeccgoto_const(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_26(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_26_(Stack),
 yeccpars2(27, Cat, [26 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_27(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 31, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_28: see yeccpars2_18

yeccpars2_29(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_29_(Stack),
 yeccpars2(30, Cat, [29 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_30_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_constants(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_31(S, decl, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_31_(Stack),
 yeccpars2(32, Cat, [31 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_32_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_33(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_33_(Stack),
 yeccpars2(yeccgoto_type(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_34(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_34_(Stack),
 yeccpars2(35, Cat, [34 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_35(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr).

yeccpars2_36(S, decl, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_36_(Stack),
 yeccpars2(37, Cat, [36 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_37_(Stack),
 Nss = lists:nthtail(4, Ss),
 yeccpars2(yeccgoto_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_38(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_38_$end'(Stack),
 yeccpars2(yeccgoto_expr(hd(Ss)), '$end', Ss, NewStack, T, Ts, Tzr);
yeccpars2_38(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_38_)'(Stack),
 yeccpars2(yeccgoto_expr(hd(Ss)), ')', Ss, NewStack, T, Ts, Tzr);
yeccpars2_38(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_38_*'(Stack),
 yeccpars2(yeccgoto_expr(hd(Ss)), '*', Ss, NewStack, T, Ts, Tzr);
yeccpars2_38(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_38_+'(Stack),
 yeccpars2(yeccgoto_expr(hd(Ss)), '+', Ss, NewStack, T, Ts, Tzr);
yeccpars2_38(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_38_,'(Stack),
 yeccpars2(yeccgoto_expr(hd(Ss)), ',', Ss, NewStack, T, Ts, Tzr);
yeccpars2_38(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_38_-'(Stack),
 yeccpars2(yeccgoto_expr(hd(Ss)), '-', Ss, NewStack, T, Ts, Tzr);
yeccpars2_38(_S, 'of', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_38_of(Stack),
 yeccpars2(yeccgoto_expr(hd(Ss)), 'of', Ss, NewStack, T, Ts, Tzr);
yeccpars2_38(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_38_|'(Stack),
 yeccpars2(yeccgoto_expr(hd(Ss)), '|', Ss, NewStack, T, Ts, Tzr);
yeccpars2_38(_S, '||', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_38_||'(Stack),
 yeccpars2(yeccgoto_expr(hd(Ss)), '||', Ss, NewStack, T, Ts, Tzr);
yeccpars2_38(_S, '|||', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_38_|||'(Stack),
 yeccpars2(yeccgoto_expr(hd(Ss)), '|||', Ss, NewStack, T, Ts, Tzr);
yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_38_(Stack),
 yeccpars2(yeccgoto_regvar(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_39(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 46, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(S, 'of', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(S, '|||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr).

yeccpars2_40(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr).

yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_41_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_cast_op(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_42: see yeccpars2_0

%% yeccpars2_43: see yeccpars2_0

%% yeccpars2_44: see yeccpars2_0

%% yeccpars2_45: see yeccpars2_0

yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_46_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_47_(Stack),
 yeccpars2(yeccgoto_mult_op(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_48_(Stack),
 yeccpars2(yeccgoto_add_op(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_49_(Stack),
 yeccpars2(yeccgoto_add_op(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_50_(Stack),
 yeccpars2(yeccgoto_path_op(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_51_(Stack),
 yeccpars2(yeccgoto_restr_op(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_52_(Stack),
 yeccpars2(yeccgoto_restr_op(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_53_(Stack),
 yeccpars2(yeccgoto_restr_op(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_54(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, 'of', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(S, '|||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_54_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_55(S, 'of', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, '|||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_55_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_56(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_56_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_57(S, 'of', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_57_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_58_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_59_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_60(S, 'of', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(S, '|||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_60_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_61_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_expr(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_62(S, atom, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_62(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr).

yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_63_(Stack),
 yeccpars2(yeccgoto_regvar(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_regatom(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_65(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr).

yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_66_(Stack),
 yeccpars2(yeccgoto_regatom(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_67(S, integer, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, string, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, var, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr).

yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_regint(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2(yeccgoto_regint(hd(Ss)), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_70(S, decl, Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 33, Ss, Stack, T, Ts, Tzr);
yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_70_(Stack),
 yeccpars2(72, Cat, [70 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_71_(Stack),
 yeccpars2(yeccgoto_regint(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_72_(Stack),
 Nss = lists:nthtail(5, Ss),
 yeccpars2(yeccgoto_regexp(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_73_(Stack),
 Nss = tl(Ss),
 yeccpars2(yeccgoto_regexp(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_74: see yeccpars2_0

yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_75_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_statements(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_76: see yeccpars2_0

yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_77_(Stack),
 yeccpars2(yeccgoto_assign_op(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_78_(Stack),
 yeccpars2(yeccgoto_assign_op(hd(Ss)), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_79(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 47, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 48, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, 'of', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 51, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, '||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 52, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(S, '|||', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_79_(Stack),
 Nss = lists:nthtail(2, Ss),
 yeccpars2(yeccgoto_statement(hd(Nss)), Cat, Nss, NewStack, T, Ts, Tzr).

yeccgoto_add_op(11) -> 45;
yeccgoto_add_op(39) -> 45;
yeccgoto_add_op(54) -> 45;
yeccgoto_add_op(55) -> 45;
yeccgoto_add_op(56) -> 45;
yeccgoto_add_op(57) -> 45;
yeccgoto_add_op(58) -> 45;
yeccgoto_add_op(60) -> 45;
yeccgoto_add_op(61) -> 45;
yeccgoto_add_op(79) -> 45.

yeccgoto_assign_op(2) -> 76.

yeccgoto_cast_op(0) -> 15;
yeccgoto_cast_op(9) -> 15;
yeccgoto_cast_op(12) -> 15;
yeccgoto_cast_op(15) -> 15;
yeccgoto_cast_op(17) -> 15;
yeccgoto_cast_op(42) -> 15;
yeccgoto_cast_op(43) -> 15;
yeccgoto_cast_op(44) -> 15;
yeccgoto_cast_op(45) -> 15;
yeccgoto_cast_op(74) -> 15;
yeccgoto_cast_op(76) -> 15.

yeccgoto_const(0) -> 14;
yeccgoto_const(9) -> 14;
yeccgoto_const(12) -> 14;
yeccgoto_const(15) -> 14;
yeccgoto_const(17) -> 14;
yeccgoto_const(18) -> 14;
yeccgoto_const(24) -> 14;
yeccgoto_const(28) -> 14;
yeccgoto_const(42) -> 14;
yeccgoto_const(43) -> 14;
yeccgoto_const(44) -> 14;
yeccgoto_const(45) -> 14;
yeccgoto_const(74) -> 14;
yeccgoto_const(76) -> 14.

yeccgoto_constant(0) -> 13;
yeccgoto_constant(9) -> 13;
yeccgoto_constant(12) -> 13;
yeccgoto_constant(15) -> 13;
yeccgoto_constant(17) -> 13;
yeccgoto_constant(18) -> 34;
yeccgoto_constant(24) -> 26;
yeccgoto_constant(28) -> 29;
yeccgoto_constant(42) -> 13;
yeccgoto_constant(43) -> 13;
yeccgoto_constant(44) -> 13;
yeccgoto_constant(45) -> 13;
yeccgoto_constant(74) -> 13;
yeccgoto_constant(76) -> 13.

yeccgoto_constants(26) -> 27;
yeccgoto_constants(29) -> 30;
yeccgoto_constants(34) -> 35.

yeccgoto_count_op(0) -> 12;
yeccgoto_count_op(9) -> 12;
yeccgoto_count_op(12) -> 12;
yeccgoto_count_op(15) -> 12;
yeccgoto_count_op(17) -> 12;
yeccgoto_count_op(42) -> 12;
yeccgoto_count_op(43) -> 12;
yeccgoto_count_op(44) -> 12;
yeccgoto_count_op(45) -> 12;
yeccgoto_count_op(74) -> 12;
yeccgoto_count_op(76) -> 12.

yeccgoto_expr(0) -> 11;
yeccgoto_expr(9) -> 61;
yeccgoto_expr(12) -> 60;
yeccgoto_expr(15) -> 58;
yeccgoto_expr(17) -> 39;
yeccgoto_expr(42) -> 57;
yeccgoto_expr(43) -> 56;
yeccgoto_expr(44) -> 55;
yeccgoto_expr(45) -> 54;
yeccgoto_expr(74) -> 11;
yeccgoto_expr(76) -> 79.

yeccgoto_id(0) -> 10;
yeccgoto_id(9) -> 10;
yeccgoto_id(12) -> 10;
yeccgoto_id(15) -> 10;
yeccgoto_id(17) -> 10;
yeccgoto_id(18) -> 25;
yeccgoto_id(24) -> 25;
yeccgoto_id(28) -> 25;
yeccgoto_id(42) -> 10;
yeccgoto_id(43) -> 10;
yeccgoto_id(44) -> 10;
yeccgoto_id(45) -> 10;
yeccgoto_id(62) -> 66;
yeccgoto_id(74) -> 10;
yeccgoto_id(76) -> 10.

yeccgoto_mult_op(11) -> 44;
yeccgoto_mult_op(39) -> 44;
yeccgoto_mult_op(54) -> 44;
yeccgoto_mult_op(55) -> 44;
yeccgoto_mult_op(56) -> 44;
yeccgoto_mult_op(57) -> 44;
yeccgoto_mult_op(58) -> 44;
yeccgoto_mult_op(60) -> 44;
yeccgoto_mult_op(61) -> 44;
yeccgoto_mult_op(79) -> 44.

yeccgoto_path_op(11) -> 43;
yeccgoto_path_op(39) -> 43;
yeccgoto_path_op(54) -> 43;
yeccgoto_path_op(55) -> 43;
yeccgoto_path_op(56) -> 43;
yeccgoto_path_op(57) -> 43;
yeccgoto_path_op(58) -> 43;
yeccgoto_path_op(60) -> 43;
yeccgoto_path_op(61) -> 43;
yeccgoto_path_op(79) -> 43.

yeccgoto_prefix_op(0) -> 9;
yeccgoto_prefix_op(9) -> 9;
yeccgoto_prefix_op(12) -> 9;
yeccgoto_prefix_op(15) -> 9;
yeccgoto_prefix_op(17) -> 9;
yeccgoto_prefix_op(42) -> 9;
yeccgoto_prefix_op(43) -> 9;
yeccgoto_prefix_op(44) -> 9;
yeccgoto_prefix_op(45) -> 9;
yeccgoto_prefix_op(74) -> 9;
yeccgoto_prefix_op(76) -> 9.

yeccgoto_regatom(0) -> 8;
yeccgoto_regatom(9) -> 8;
yeccgoto_regatom(12) -> 8;
yeccgoto_regatom(15) -> 8;
yeccgoto_regatom(17) -> 8;
yeccgoto_regatom(42) -> 8;
yeccgoto_regatom(43) -> 8;
yeccgoto_regatom(44) -> 8;
yeccgoto_regatom(45) -> 8;
yeccgoto_regatom(62) -> 65;
yeccgoto_regatom(74) -> 8;
yeccgoto_regatom(76) -> 8.

yeccgoto_regexp(0) -> 7;
yeccgoto_regexp(9) -> 7;
yeccgoto_regexp(12) -> 7;
yeccgoto_regexp(15) -> 7;
yeccgoto_regexp(17) -> 7;
yeccgoto_regexp(42) -> 7;
yeccgoto_regexp(43) -> 7;
yeccgoto_regexp(44) -> 7;
yeccgoto_regexp(45) -> 7;
yeccgoto_regexp(74) -> 7;
yeccgoto_regexp(76) -> 7.

yeccgoto_regint(67) -> 70.

yeccgoto_regstr(0) -> 6;
yeccgoto_regstr(9) -> 6;
yeccgoto_regstr(12) -> 6;
yeccgoto_regstr(15) -> 6;
yeccgoto_regstr(17) -> 6;
yeccgoto_regstr(42) -> 6;
yeccgoto_regstr(43) -> 6;
yeccgoto_regstr(44) -> 6;
yeccgoto_regstr(45) -> 6;
yeccgoto_regstr(62) -> 64;
yeccgoto_regstr(67) -> 69;
yeccgoto_regstr(74) -> 6;
yeccgoto_regstr(76) -> 6.

yeccgoto_regvar(0) -> 5;
yeccgoto_regvar(9) -> 5;
yeccgoto_regvar(12) -> 5;
yeccgoto_regvar(15) -> 5;
yeccgoto_regvar(17) -> 5;
yeccgoto_regvar(42) -> 5;
yeccgoto_regvar(43) -> 5;
yeccgoto_regvar(44) -> 5;
yeccgoto_regvar(45) -> 5;
yeccgoto_regvar(62) -> 5;
yeccgoto_regvar(67) -> 68;
yeccgoto_regvar(74) -> 5;
yeccgoto_regvar(76) -> 5.

yeccgoto_restr_op(11) -> 42;
yeccgoto_restr_op(39) -> 42;
yeccgoto_restr_op(54) -> 42;
yeccgoto_restr_op(55) -> 42;
yeccgoto_restr_op(56) -> 42;
yeccgoto_restr_op(57) -> 42;
yeccgoto_restr_op(58) -> 42;
yeccgoto_restr_op(60) -> 42;
yeccgoto_restr_op(61) -> 42;
yeccgoto_restr_op(79) -> 42.

yeccgoto_statement(0) -> 4;
yeccgoto_statement(74) -> 4.

yeccgoto_statements(0) -> 3;
yeccgoto_statements(74) -> 75.

yeccgoto_type(6) -> 73;
yeccgoto_type(13) -> 59;
yeccgoto_type(31) -> 32;
yeccgoto_type(36) -> 37;
yeccgoto_type(70) -> 72.

yeccgoto_variable(0) -> 2;
yeccgoto_variable(9) -> 38;
yeccgoto_variable(12) -> 38;
yeccgoto_variable(15) -> 38;
yeccgoto_variable(17) -> 38;
yeccgoto_variable(42) -> 38;
yeccgoto_variable(43) -> 38;
yeccgoto_variable(44) -> 38;
yeccgoto_variable(45) -> 38;
yeccgoto_variable(62) -> 63;
yeccgoto_variable(67) -> 63;
yeccgoto_variable(74) -> 2;
yeccgoto_variable(76) -> 38.

yeccgoto_xref(0) -> 1.

-compile({inline,{'yeccpars2_2_$end',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_2_$end'([__1 | Stack]) ->
 [begin
   { variable , __1 }
  end | Stack].

-compile({inline,{'yeccpars2_2_*',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_2_*'([__1 | Stack]) ->
 [begin
   { variable , __1 }
  end | Stack].

-compile({inline,{'yeccpars2_2_+',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_2_+'([__1 | Stack]) ->
 [begin
   { variable , __1 }
  end | Stack].

-compile({inline,{'yeccpars2_2_-',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_2_-'([__1 | Stack]) ->
 [begin
   { variable , __1 }
  end | Stack].

-compile({inline,{yeccpars2_2_of,1}}).
-file("xref_parser.yrl", 63).
yeccpars2_2_of([__1 | Stack]) ->
 [begin
   { variable , __1 }
  end | Stack].

-compile({inline,{'yeccpars2_2_|',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_2_|'([__1 | Stack]) ->
 [begin
   { variable , __1 }
  end | Stack].

-compile({inline,{'yeccpars2_2_||',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_2_||'([__1 | Stack]) ->
 [begin
   { variable , __1 }
  end | Stack].

-compile({inline,{'yeccpars2_2_|||',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_2_|||'([__1 | Stack]) ->
 [begin
   { variable , __1 }
  end | Stack].

-compile({inline,{yeccpars2_2_,1}}).
-file("xref_parser.yrl", 96).
yeccpars2_2_([__1 | Stack]) ->
 [begin
   check_regexp_variable ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_4_,1}}).
-file("xref_parser.yrl", 54).
yeccpars2_4_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{'yeccpars2_6_$end',1}}).
-file("xref_parser.yrl", 102).
'yeccpars2_6_$end'(Stack) ->
 [begin
   unknown
  end | Stack].

-compile({inline,{'yeccpars2_6_)',1}}).
-file("xref_parser.yrl", 102).
'yeccpars2_6_)'(Stack) ->
 [begin
   unknown
  end | Stack].

-compile({inline,{'yeccpars2_6_*',1}}).
-file("xref_parser.yrl", 102).
'yeccpars2_6_*'(Stack) ->
 [begin
   unknown
  end | Stack].

-compile({inline,{'yeccpars2_6_+',1}}).
-file("xref_parser.yrl", 102).
'yeccpars2_6_+'(Stack) ->
 [begin
   unknown
  end | Stack].

-compile({inline,{'yeccpars2_6_,',1}}).
-file("xref_parser.yrl", 102).
'yeccpars2_6_,'(Stack) ->
 [begin
   unknown
  end | Stack].

-compile({inline,{'yeccpars2_6_-',1}}).
-file("xref_parser.yrl", 102).
'yeccpars2_6_-'(Stack) ->
 [begin
   unknown
  end | Stack].

-compile({inline,{yeccpars2_6_of,1}}).
-file("xref_parser.yrl", 102).
yeccpars2_6_of(Stack) ->
 [begin
   unknown
  end | Stack].

-compile({inline,{'yeccpars2_6_|',1}}).
-file("xref_parser.yrl", 102).
'yeccpars2_6_|'(Stack) ->
 [begin
   unknown
  end | Stack].

-compile({inline,{'yeccpars2_6_||',1}}).
-file("xref_parser.yrl", 102).
'yeccpars2_6_||'(Stack) ->
 [begin
   unknown
  end | Stack].

-compile({inline,{'yeccpars2_6_|||',1}}).
-file("xref_parser.yrl", 102).
'yeccpars2_6_|||'(Stack) ->
 [begin
   unknown
  end | Stack].

-compile({inline,{'yeccpars2_10_$end',1}}).
-file("xref_parser.yrl", 79).
'yeccpars2_10_$end'([__1 | Stack]) ->
 [begin
   { constant , unknown , vertex , __1 }
  end | Stack].

-compile({inline,{'yeccpars2_10_)',1}}).
-file("xref_parser.yrl", 79).
'yeccpars2_10_)'([__1 | Stack]) ->
 [begin
   { constant , unknown , vertex , __1 }
  end | Stack].

-compile({inline,{'yeccpars2_10_*',1}}).
-file("xref_parser.yrl", 79).
'yeccpars2_10_*'([__1 | Stack]) ->
 [begin
   { constant , unknown , vertex , __1 }
  end | Stack].

-compile({inline,{'yeccpars2_10_+',1}}).
-file("xref_parser.yrl", 79).
'yeccpars2_10_+'([__1 | Stack]) ->
 [begin
   { constant , unknown , vertex , __1 }
  end | Stack].

-compile({inline,{'yeccpars2_10_,',1}}).
-file("xref_parser.yrl", 79).
'yeccpars2_10_,'([__1 | Stack]) ->
 [begin
   { constant , unknown , vertex , __1 }
  end | Stack].

-compile({inline,{'yeccpars2_10_-',1}}).
-file("xref_parser.yrl", 79).
'yeccpars2_10_-'([__1 | Stack]) ->
 [begin
   { constant , unknown , vertex , __1 }
  end | Stack].

-compile({inline,{yeccpars2_10_decl,1}}).
-file("xref_parser.yrl", 79).
yeccpars2_10_decl([__1 | Stack]) ->
 [begin
   { constant , unknown , vertex , __1 }
  end | Stack].

-compile({inline,{yeccpars2_10_of,1}}).
-file("xref_parser.yrl", 79).
yeccpars2_10_of([__1 | Stack]) ->
 [begin
   { constant , unknown , vertex , __1 }
  end | Stack].

-compile({inline,{'yeccpars2_10_|',1}}).
-file("xref_parser.yrl", 79).
'yeccpars2_10_|'([__1 | Stack]) ->
 [begin
   { constant , unknown , vertex , __1 }
  end | Stack].

-compile({inline,{'yeccpars2_10_||',1}}).
-file("xref_parser.yrl", 79).
'yeccpars2_10_||'([__1 | Stack]) ->
 [begin
   { constant , unknown , vertex , __1 }
  end | Stack].

-compile({inline,{'yeccpars2_10_|||',1}}).
-file("xref_parser.yrl", 79).
'yeccpars2_10_|||'([__1 | Stack]) ->
 [begin
   { constant , unknown , vertex , __1 }
  end | Stack].

-compile({inline,{yeccpars2_10_,1}}).
-file("xref_parser.yrl", 88).
yeccpars2_10_([__1 | Stack]) ->
 [begin
   { atom , __1 }
  end | Stack].

-compile({inline,{yeccpars2_11_,1}}).
-file("xref_parser.yrl", 55).
yeccpars2_11_([__1 | Stack]) ->
 [begin
   [ __1 ]
  end | Stack].

-compile({inline,{yeccpars2_13_,1}}).
-file("xref_parser.yrl", 102).
yeccpars2_13_(Stack) ->
 [begin
   unknown
  end | Stack].

-compile({inline,{yeccpars2_16_,1}}).
-file("xref_parser.yrl", 38).
yeccpars2_16_([__1 | Stack]) ->
 [begin
   '#'
  end | Stack].

-compile({inline,{yeccpars2_19_,1}}).
-file("xref_parser.yrl", 98).
yeccpars2_19_([__1 | Stack]) ->
 [begin
   value_of ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_20_,1}}).
-file("xref_parser.yrl", 80).
yeccpars2_20_([__1 | Stack]) ->
 [begin
   value_of ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_21_,1}}).
-file("xref_parser.yrl", 95).
yeccpars2_21_([__1 | Stack]) ->
 [begin
   check_regexp ( value_of ( __1 ) )
  end | Stack].

-compile({inline,{yeccpars2_22_,1}}).
-file("xref_parser.yrl", 99).
yeccpars2_22_([__1 | Stack]) ->
 [begin
   value_of ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_23_,1}}).
-file("xref_parser.yrl", 81).
yeccpars2_23_([__1 | Stack]) ->
 [begin
   value_of ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_25_,1}}).
-file("xref_parser.yrl", 79).
yeccpars2_25_([__1 | Stack]) ->
 [begin
   { constant , unknown , vertex , __1 }
  end | Stack].

-compile({inline,{yeccpars2_26_,1}}).
-file("xref_parser.yrl", 74).
yeccpars2_26_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_29_,1}}).
-file("xref_parser.yrl", 74).
yeccpars2_29_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_30_,1}}).
-file("xref_parser.yrl", 75).
yeccpars2_30_([__3,__2,__1 | Stack]) ->
 [begin
   [ __2 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_31_,1}}).
-file("xref_parser.yrl", 102).
yeccpars2_31_(Stack) ->
 [begin
   unknown
  end | Stack].

-compile({inline,{yeccpars2_32_,1}}).
-file("xref_parser.yrl", 61).
yeccpars2_32_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   type ( { tuple , [ __2 | __3 ] } , __5 )
  end | Stack].

-compile({inline,{yeccpars2_33_,1}}).
-file("xref_parser.yrl", 101).
yeccpars2_33_([__1 | Stack]) ->
 [begin
   value_of ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_34_,1}}).
-file("xref_parser.yrl", 74).
yeccpars2_34_(Stack) ->
 [begin
   [ ]
  end | Stack].

-compile({inline,{yeccpars2_36_,1}}).
-file("xref_parser.yrl", 102).
yeccpars2_36_(Stack) ->
 [begin
   unknown
  end | Stack].

-compile({inline,{yeccpars2_37_,1}}).
-file("xref_parser.yrl", 60).
yeccpars2_37_([__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   type ( { list , [ __2 | __3 ] } , __5 )
  end | Stack].

-compile({inline,{'yeccpars2_38_$end',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_38_$end'([__1 | Stack]) ->
 [begin
   { variable , __1 }
  end | Stack].

-compile({inline,{'yeccpars2_38_)',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_38_)'([__1 | Stack]) ->
 [begin
   { variable , __1 }
  end | Stack].

-compile({inline,{'yeccpars2_38_*',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_38_*'([__1 | Stack]) ->
 [begin
   { variable , __1 }
  end | Stack].

-compile({inline,{'yeccpars2_38_+',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_38_+'([__1 | Stack]) ->
 [begin
   { variable , __1 }
  end | Stack].

-compile({inline,{'yeccpars2_38_,',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_38_,'([__1 | Stack]) ->
 [begin
   { variable , __1 }
  end | Stack].

-compile({inline,{'yeccpars2_38_-',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_38_-'([__1 | Stack]) ->
 [begin
   { variable , __1 }
  end | Stack].

-compile({inline,{yeccpars2_38_of,1}}).
-file("xref_parser.yrl", 63).
yeccpars2_38_of([__1 | Stack]) ->
 [begin
   { variable , __1 }
  end | Stack].

-compile({inline,{'yeccpars2_38_|',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_38_|'([__1 | Stack]) ->
 [begin
   { variable , __1 }
  end | Stack].

-compile({inline,{'yeccpars2_38_||',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_38_||'([__1 | Stack]) ->
 [begin
   { variable , __1 }
  end | Stack].

-compile({inline,{'yeccpars2_38_|||',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_38_|||'([__1 | Stack]) ->
 [begin
   { variable , __1 }
  end | Stack].

-compile({inline,{yeccpars2_38_,1}}).
-file("xref_parser.yrl", 96).
yeccpars2_38_([__1 | Stack]) ->
 [begin
   check_regexp_variable ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_41_,1}}).
-file("xref_parser.yrl", 43).
yeccpars2_41_([__3,__2,__1 | Stack]) ->
 [begin
   value_of ( __2 )
  end | Stack].

-compile({inline,{yeccpars2_46_,1}}).
-file("xref_parser.yrl", 72).
yeccpars2_46_([__3,__2,__1 | Stack]) ->
 [begin
   __2
  end | Stack].

-compile({inline,{yeccpars2_47_,1}}).
-file("xref_parser.yrl", 37).
yeccpars2_47_([__1 | Stack]) ->
 [begin
   intersection
  end | Stack].

-compile({inline,{yeccpars2_48_,1}}).
-file("xref_parser.yrl", 35).
yeccpars2_48_([__1 | Stack]) ->
 [begin
   union
  end | Stack].

-compile({inline,{yeccpars2_49_,1}}).
-file("xref_parser.yrl", 36).
yeccpars2_49_([__1 | Stack]) ->
 [begin
   difference
  end | Stack].

-compile({inline,{yeccpars2_50_,1}}).
-file("xref_parser.yrl", 42).
yeccpars2_50_([__1 | Stack]) ->
 [begin
   'of'
  end | Stack].

-compile({inline,{yeccpars2_51_,1}}).
-file("xref_parser.yrl", 39).
yeccpars2_51_([__1 | Stack]) ->
 [begin
   '|'
  end | Stack].

-compile({inline,{yeccpars2_52_,1}}).
-file("xref_parser.yrl", 40).
yeccpars2_52_([__1 | Stack]) ->
 [begin
   '||'
  end | Stack].

-compile({inline,{yeccpars2_53_,1}}).
-file("xref_parser.yrl", 41).
yeccpars2_53_([__1 | Stack]) ->
 [begin
   '|||'
  end | Stack].

-compile({inline,{yeccpars2_54_,1}}).
-file("xref_parser.yrl", 64).
yeccpars2_54_([__3,__2,__1 | Stack]) ->
 [begin
   { set , __2 , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_55_,1}}).
-file("xref_parser.yrl", 65).
yeccpars2_55_([__3,__2,__1 | Stack]) ->
 [begin
   { set , __2 , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_56_,1}}).
-file("xref_parser.yrl", 68).
yeccpars2_56_([__3,__2,__1 | Stack]) ->
 [begin
   { path , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_57_,1}}).
-file("xref_parser.yrl", 67).
yeccpars2_57_([__3,__2,__1 | Stack]) ->
 [begin
   { restr , __2 , __1 , __3 }
  end | Stack].

-compile({inline,{yeccpars2_58_,1}}).
-file("xref_parser.yrl", 69).
yeccpars2_58_([__2,__1 | Stack]) ->
 [begin
   { type , { convert , __1 } , __2 }
  end | Stack].

-compile({inline,{yeccpars2_59_,1}}).
-file("xref_parser.yrl", 62).
yeccpars2_59_([__2,__1 | Stack]) ->
 [begin
   type ( __1 , __2 )
  end | Stack].

-compile({inline,{yeccpars2_60_,1}}).
-file("xref_parser.yrl", 66).
yeccpars2_60_([__2,__1 | Stack]) ->
 [begin
   prefix ( __1 , __2 )
  end | Stack].

-compile({inline,{yeccpars2_61_,1}}).
-file("xref_parser.yrl", 70).
yeccpars2_61_([__2,__1 | Stack]) ->
 [begin
   prefix ( __1 , __2 )
  end | Stack].

-compile({inline,{yeccpars2_63_,1}}).
-file("xref_parser.yrl", 96).
yeccpars2_63_([__1 | Stack]) ->
 [begin
   check_regexp_variable ( __1 )
  end | Stack].

-compile({inline,{yeccpars2_66_,1}}).
-file("xref_parser.yrl", 88).
yeccpars2_66_([__1 | Stack]) ->
 [begin
   { atom , __1 }
  end | Stack].

-compile({inline,{yeccpars2_70_,1}}).
-file("xref_parser.yrl", 102).
yeccpars2_70_(Stack) ->
 [begin
   unknown
  end | Stack].

-compile({inline,{yeccpars2_71_,1}}).
-file("xref_parser.yrl", 92).
yeccpars2_71_([__1 | Stack]) ->
 [begin
   { integer , value_of ( __1 ) }
  end | Stack].

-compile({inline,{yeccpars2_72_,1}}).
-file("xref_parser.yrl", 85).
yeccpars2_72_([__6,__5,__4,__3,__2,__1 | Stack]) ->
 [begin
   regexp ( func , { __1 , __3 , __5 } , __6 )
  end | Stack].

-compile({inline,{yeccpars2_73_,1}}).
-file("xref_parser.yrl", 83).
yeccpars2_73_([__2,__1 | Stack]) ->
 [begin
   regexp ( atom , __1 , __2 )
  end | Stack].

-compile({inline,{yeccpars2_75_,1}}).
-file("xref_parser.yrl", 56).
yeccpars2_75_([__3,__2,__1 | Stack]) ->
 [begin
   [ __1 | __3 ]
  end | Stack].

-compile({inline,{yeccpars2_77_,1}}).
-file("xref_parser.yrl", 34).
yeccpars2_77_([__1 | Stack]) ->
 [begin
   user
  end | Stack].

-compile({inline,{yeccpars2_78_,1}}).
-file("xref_parser.yrl", 33).
yeccpars2_78_([__1 | Stack]) ->
 [begin
   tmp
  end | Stack].

-compile({inline,{yeccpars2_79_,1}}).
-file("xref_parser.yrl", 58).
yeccpars2_79_([__3,__2,__1 | Stack]) ->
 [begin
   { assign , __2 , __1 , __3 }
  end | Stack].


-file("xref_parser.yrl", 304).
