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

-file("/ldisk/daily_build/otp_prebuild_r11b.2007-06-11_19/otp_src_R11B-5/bootstrap/lib/parsetools/include/yeccpre.hrl", 0).
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
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

yeccpars0(Tokens, MFA) ->
    try yeccpars1(Tokens, MFA, 0, [], [])
    catch 
        throw: {error, {_Line, ?MODULE, _M}} = Error -> 
                   Error % probably from return_error/1
    end.

% Don't change yeccpars1/6 too much, it is called recursively by yeccpars2/8!
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



-file("./xref_parser.erl", 294).

yeccpars2(0, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, edge, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, vertex, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [0 | __Ss], [__T | __Stack]);
yeccpars2(0, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(1, '$end', _, __Stack, _, _, _) ->
 {ok, hd(__Stack)};
yeccpars2(1, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(2, ':=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 77, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, '=', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 78, [2 | __Ss], [__T | __Stack]);
yeccpars2(2, '$end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_2_$end'(__Stack),
 yeccpars2(yeccgoto(expr, hd(__Ss)), '$end', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(2, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_2_*'(__Stack),
 yeccpars2(yeccgoto(expr, hd(__Ss)), '*', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(2, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_2_+'(__Stack),
 yeccpars2(yeccgoto(expr, hd(__Ss)), '+', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(2, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_2_-'(__Stack),
 yeccpars2(yeccgoto(expr, hd(__Ss)), '-', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(2, 'of', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_2_of(__Stack),
 yeccpars2(yeccgoto(expr, hd(__Ss)), 'of', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(2, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_2_|'(__Stack),
 yeccpars2(yeccgoto(expr, hd(__Ss)), '|', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(2, '||', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_2_||'(__Stack),
 yeccpars2(yeccgoto(expr, hd(__Ss)), '||', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(2, '|||', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_2_|||'(__Stack),
 yeccpars2(yeccgoto(expr, hd(__Ss)), '|||', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(2, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_2_(__Stack),
 yeccpars2(yeccgoto(regvar, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(3, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(xref, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(4, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 74, [4 | __Ss], [__T | __Stack]);
yeccpars2(4, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_4_(__Stack),
 yeccpars2(yeccgoto(statements, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(5, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(regatom, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(6, decl, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [6 | __Ss], [__T | __Stack]);
yeccpars2(6, '$end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_6_$end'(__Stack),
 yeccpars2(73, '$end', [6 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(6, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_6_)'(__Stack),
 yeccpars2(73, ')', [6 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(6, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_6_*'(__Stack),
 yeccpars2(73, '*', [6 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(6, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_6_+'(__Stack),
 yeccpars2(73, '+', [6 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(6, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_6_,'(__Stack),
 yeccpars2(73, ',', [6 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(6, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_6_-'(__Stack),
 yeccpars2(73, '-', [6 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(6, 'of', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_6_of(__Stack),
 yeccpars2(73, 'of', [6 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(6, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_6_|'(__Stack),
 yeccpars2(73, '|', [6 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(6, '||', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_6_||'(__Stack),
 yeccpars2(73, '||', [6 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(6, '|||', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_6_|||'(__Stack),
 yeccpars2(73, '|||', [6 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(6, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(regatom, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(7, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(expr, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(8, ':', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 62, [8 | __Ss], [__T | __Stack]);
yeccpars2(8, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(9, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, edge, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, vertex, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [9 | __Ss], [__T | __Stack]);
yeccpars2(9, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(10, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(prefix_op, hd(__Ss)), '#', __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(10, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(prefix_op, hd(__Ss)), '(', __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(10, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(prefix_op, hd(__Ss)), '[', __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(10, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(prefix_op, hd(__Ss)), atom, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(10, edge, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(prefix_op, hd(__Ss)), edge, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(10, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(prefix_op, hd(__Ss)), string, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(10, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(prefix_op, hd(__Ss)), var, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(10, vertex, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(prefix_op, hd(__Ss)), vertex, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(10, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(prefix_op, hd(__Ss)), '{', __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(10, '$end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_10_$end'(__Stack),
 yeccpars2(yeccgoto(const, hd(__Ss)), '$end', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(10, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_10_)'(__Stack),
 yeccpars2(yeccgoto(const, hd(__Ss)), ')', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(10, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_10_*'(__Stack),
 yeccpars2(yeccgoto(const, hd(__Ss)), '*', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(10, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_10_+'(__Stack),
 yeccpars2(yeccgoto(const, hd(__Ss)), '+', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(10, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_10_,'(__Stack),
 yeccpars2(yeccgoto(const, hd(__Ss)), ',', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(10, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_10_-'(__Stack),
 yeccpars2(yeccgoto(const, hd(__Ss)), '-', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(10, decl, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_10_decl(__Stack),
 yeccpars2(yeccgoto(const, hd(__Ss)), decl, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(10, 'of', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_10_of(__Stack),
 yeccpars2(yeccgoto(const, hd(__Ss)), 'of', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(10, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_10_|'(__Stack),
 yeccpars2(yeccgoto(const, hd(__Ss)), '|', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(10, '||', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_10_||'(__Stack),
 yeccpars2(yeccgoto(const, hd(__Ss)), '||', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(10, '|||', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_10_|||'(__Stack),
 yeccpars2(yeccgoto(const, hd(__Ss)), '|||', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(10, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_10_(__Stack),
 yeccpars2(yeccgoto(regatom, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(11, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, 'of', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, '||', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, '|||', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [11 | __Ss], [__T | __Stack]);
yeccpars2(11, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_11_(__Stack),
 yeccpars2(yeccgoto(statements, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(12, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, edge, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, vertex, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [12 | __Ss], [__T | __Stack]);
yeccpars2(12, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(13, decl, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [13 | __Ss], [__T | __Stack]);
yeccpars2(13, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_13_(__Stack),
 yeccpars2(59, __Cat, [13 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(14, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(constant, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(15, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, edge, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, vertex, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [15 | __Ss], [__T | __Stack]);
yeccpars2(15, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(16, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_16_(__Stack),
 yeccpars2(yeccgoto(count_op, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(17, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, cast, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 40, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, edge, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, vertex, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [17 | __Ss], [__T | __Stack]);
yeccpars2(17, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(18, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, edge, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, vertex, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [18 | __Ss], [__T | __Stack]);
yeccpars2(18, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(19, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_19_(__Stack),
 yeccpars2(yeccgoto(id, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(20, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_20_(__Stack),
 yeccpars2(yeccgoto(const, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(21, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_21_(__Stack),
 yeccpars2(yeccgoto(regstr, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(22, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_22_(__Stack),
 yeccpars2(yeccgoto(variable, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(23, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_23_(__Stack),
 yeccpars2(yeccgoto(const, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(24, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, edge, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, vertex, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [24 | __Ss], [__T | __Stack]);
yeccpars2(24, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(25, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_25_(__Stack),
 yeccpars2(yeccgoto(const, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(26, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [26 | __Ss], [__T | __Stack]);
yeccpars2(26, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_26_(__Stack),
 yeccpars2(27, __Cat, [26 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(27, '}', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 31, [27 | __Ss], [__T | __Stack]);
yeccpars2(27, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(28, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, edge, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, vertex, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [28 | __Ss], [__T | __Stack]);
yeccpars2(28, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(29, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [29 | __Ss], [__T | __Stack]);
yeccpars2(29, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_29_(__Stack),
 yeccpars2(30, __Cat, [29 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(30, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_30_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(constants, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(31, decl, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [31 | __Ss], [__T | __Stack]);
yeccpars2(31, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_31_(__Stack),
 yeccpars2(32, __Cat, [31 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(32, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_32_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(33, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_33_(__Stack),
 yeccpars2(yeccgoto(type, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(34, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 28, [34 | __Ss], [__T | __Stack]);
yeccpars2(34, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_34_(__Stack),
 yeccpars2(35, __Cat, [34 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(35, ']', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 36, [35 | __Ss], [__T | __Stack]);
yeccpars2(35, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(36, decl, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [36 | __Ss], [__T | __Stack]);
yeccpars2(36, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_36_(__Stack),
 yeccpars2(37, __Cat, [36 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(37, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_37_(__Stack),
 __Nss = lists:nthtail(4, __Ss),
 yeccpars2(yeccgoto(expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(38, '$end', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_38_$end'(__Stack),
 yeccpars2(yeccgoto(expr, hd(__Ss)), '$end', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(38, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_38_)'(__Stack),
 yeccpars2(yeccgoto(expr, hd(__Ss)), ')', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(38, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_38_*'(__Stack),
 yeccpars2(yeccgoto(expr, hd(__Ss)), '*', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(38, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_38_+'(__Stack),
 yeccpars2(yeccgoto(expr, hd(__Ss)), '+', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(38, ',', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_38_,'(__Stack),
 yeccpars2(yeccgoto(expr, hd(__Ss)), ',', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(38, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_38_-'(__Stack),
 yeccpars2(yeccgoto(expr, hd(__Ss)), '-', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(38, 'of', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_38_of(__Stack),
 yeccpars2(yeccgoto(expr, hd(__Ss)), 'of', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(38, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_38_|'(__Stack),
 yeccpars2(yeccgoto(expr, hd(__Ss)), '|', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(38, '||', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_38_||'(__Stack),
 yeccpars2(yeccgoto(expr, hd(__Ss)), '||', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(38, '|||', __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = 'yeccpars2_38_|||'(__Stack),
 yeccpars2(yeccgoto(expr, hd(__Ss)), '|||', __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(38, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_38_(__Stack),
 yeccpars2(yeccgoto(regvar, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(39, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 46, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, 'of', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '||', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, '|||', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [39 | __Ss], [__T | __Stack]);
yeccpars2(39, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(40, ')', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 41, [40 | __Ss], [__T | __Stack]);
yeccpars2(40, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(41, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_41_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(cast_op, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(42, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, edge, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, vertex, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [42 | __Ss], [__T | __Stack]);
yeccpars2(42, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(43, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [43 | __Ss], [__T | __Stack]);
yeccpars2(43, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [43 | __Ss], [__T | __Stack]);
yeccpars2(43, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [43 | __Ss], [__T | __Stack]);
yeccpars2(43, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [43 | __Ss], [__T | __Stack]);
yeccpars2(43, edge, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [43 | __Ss], [__T | __Stack]);
yeccpars2(43, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [43 | __Ss], [__T | __Stack]);
yeccpars2(43, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [43 | __Ss], [__T | __Stack]);
yeccpars2(43, vertex, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [43 | __Ss], [__T | __Stack]);
yeccpars2(43, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [43 | __Ss], [__T | __Stack]);
yeccpars2(43, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(44, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, edge, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, vertex, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [44 | __Ss], [__T | __Stack]);
yeccpars2(44, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(45, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, edge, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, vertex, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [45 | __Ss], [__T | __Stack]);
yeccpars2(45, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(46, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_46_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(47, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_47_(__Stack),
 yeccpars2(yeccgoto(mult_op, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(48, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_48_(__Stack),
 yeccpars2(yeccgoto(add_op, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(49, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_49_(__Stack),
 yeccpars2(yeccgoto(add_op, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(50, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_50_(__Stack),
 yeccpars2(yeccgoto(path_op, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(51, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_51_(__Stack),
 yeccpars2(yeccgoto(restr_op, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(52, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_52_(__Stack),
 yeccpars2(yeccgoto(restr_op, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(53, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_53_(__Stack),
 yeccpars2(yeccgoto(restr_op, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(54, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, 'of', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, '||', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, '|||', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [54 | __Ss], [__T | __Stack]);
yeccpars2(54, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_54_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(55, 'of', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, '||', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, '|||', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [55 | __Ss], [__T | __Stack]);
yeccpars2(55, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_55_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(56, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_56_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(57, 'of', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [57 | __Ss], [__T | __Stack]);
yeccpars2(57, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_57_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(58, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_58_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(59, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_59_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(60, 'of', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, '||', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, '|||', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [60 | __Ss], [__T | __Stack]);
yeccpars2(60, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_60_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(61, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_61_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(expr, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(62, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [62 | __Ss], [__T | __Stack]);
yeccpars2(62, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [62 | __Ss], [__T | __Stack]);
yeccpars2(62, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [62 | __Ss], [__T | __Stack]);
yeccpars2(62, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(63, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_63_(__Stack),
 yeccpars2(yeccgoto(regvar, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(64, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(regatom, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(65, '/', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 67, [65 | __Ss], [__T | __Stack]);
yeccpars2(65, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(66, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_66_(__Stack),
 yeccpars2(yeccgoto(regatom, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(67, integer, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 71, [67 | __Ss], [__T | __Stack]);
yeccpars2(67, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [67 | __Ss], [__T | __Stack]);
yeccpars2(67, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [67 | __Ss], [__T | __Stack]);
yeccpars2(67, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(68, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(regint, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(69, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars2(yeccgoto(regint, hd(__Ss)), __Cat, __Ss, __Stack, __T, __Ts, __Tzr);
yeccpars2(70, decl, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 33, [70 | __Ss], [__T | __Stack]);
yeccpars2(70, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_70_(__Stack),
 yeccpars2(72, __Cat, [70 | __Ss], __NewStack, __T, __Ts, __Tzr);
yeccpars2(71, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_71_(__Stack),
 yeccpars2(yeccgoto(regint, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(72, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_72_(__Stack),
 __Nss = lists:nthtail(5, __Ss),
 yeccpars2(yeccgoto(regexp, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(73, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_73_(__Stack),
 __Nss = lists:nthtail(1, __Ss),
 yeccpars2(yeccgoto(regexp, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(74, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, edge, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, vertex, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [74 | __Ss], [__T | __Stack]);
yeccpars2(74, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(75, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_75_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(statements, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(76, '#', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 16, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, '(', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 17, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, '[', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 18, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, atom, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 19, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, edge, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 20, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, string, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 21, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, var, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 22, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, vertex, __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 23, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, '{', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 24, [76 | __Ss], [__T | __Stack]);
yeccpars2(76, _, _, _, __T, _, _) ->
 yeccerror(__T);
yeccpars2(77, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_77_(__Stack),
 yeccpars2(yeccgoto(assign_op, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(78, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_78_(__Stack),
 yeccpars2(yeccgoto(assign_op, hd(__Ss)), __Cat, __Ss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(79, '*', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 47, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '+', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 48, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '-', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 49, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, 'of', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 50, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '|', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 51, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '||', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 52, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, '|||', __Ss, __Stack, __T, __Ts, __Tzr) ->
 yeccpars1(__Ts, __Tzr, 53, [79 | __Ss], [__T | __Stack]);
yeccpars2(79, __Cat, __Ss, __Stack, __T, __Ts, __Tzr) ->
 __NewStack = yeccpars2_79_(__Stack),
 __Nss = lists:nthtail(2, __Ss),
 yeccpars2(yeccgoto(statement, hd(__Nss)), __Cat, __Nss, __NewStack, __T, __Ts, __Tzr);
yeccpars2(__Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.1",{missing_state_in_action_table, __Other}}).

yeccgoto(add_op, 11) ->
 45;
yeccgoto(add_op, 39) ->
 45;
yeccgoto(add_op, 54) ->
 45;
yeccgoto(add_op, 55) ->
 45;
yeccgoto(add_op, 56) ->
 45;
yeccgoto(add_op, 57) ->
 45;
yeccgoto(add_op, 58) ->
 45;
yeccgoto(add_op, 60) ->
 45;
yeccgoto(add_op, 61) ->
 45;
yeccgoto(add_op, 79) ->
 45;
yeccgoto(assign_op, 2) ->
 76;
yeccgoto(cast_op, 0) ->
 15;
yeccgoto(cast_op, 9) ->
 15;
yeccgoto(cast_op, 12) ->
 15;
yeccgoto(cast_op, 15) ->
 15;
yeccgoto(cast_op, 17) ->
 15;
yeccgoto(cast_op, 42) ->
 15;
yeccgoto(cast_op, 43) ->
 15;
yeccgoto(cast_op, 44) ->
 15;
yeccgoto(cast_op, 45) ->
 15;
yeccgoto(cast_op, 74) ->
 15;
yeccgoto(cast_op, 76) ->
 15;
yeccgoto(const, 0) ->
 14;
yeccgoto(const, 9) ->
 14;
yeccgoto(const, 12) ->
 14;
yeccgoto(const, 15) ->
 14;
yeccgoto(const, 17) ->
 14;
yeccgoto(const, 18) ->
 14;
yeccgoto(const, 24) ->
 14;
yeccgoto(const, 28) ->
 14;
yeccgoto(const, 42) ->
 14;
yeccgoto(const, 43) ->
 14;
yeccgoto(const, 44) ->
 14;
yeccgoto(const, 45) ->
 14;
yeccgoto(const, 74) ->
 14;
yeccgoto(const, 76) ->
 14;
yeccgoto(constant, 0) ->
 13;
yeccgoto(constant, 9) ->
 13;
yeccgoto(constant, 12) ->
 13;
yeccgoto(constant, 15) ->
 13;
yeccgoto(constant, 17) ->
 13;
yeccgoto(constant, 18) ->
 34;
yeccgoto(constant, 24) ->
 26;
yeccgoto(constant, 28) ->
 29;
yeccgoto(constant, 42) ->
 13;
yeccgoto(constant, 43) ->
 13;
yeccgoto(constant, 44) ->
 13;
yeccgoto(constant, 45) ->
 13;
yeccgoto(constant, 74) ->
 13;
yeccgoto(constant, 76) ->
 13;
yeccgoto(constants, 26) ->
 27;
yeccgoto(constants, 29) ->
 30;
yeccgoto(constants, 34) ->
 35;
yeccgoto(count_op, 0) ->
 12;
yeccgoto(count_op, 9) ->
 12;
yeccgoto(count_op, 12) ->
 12;
yeccgoto(count_op, 15) ->
 12;
yeccgoto(count_op, 17) ->
 12;
yeccgoto(count_op, 42) ->
 12;
yeccgoto(count_op, 43) ->
 12;
yeccgoto(count_op, 44) ->
 12;
yeccgoto(count_op, 45) ->
 12;
yeccgoto(count_op, 74) ->
 12;
yeccgoto(count_op, 76) ->
 12;
yeccgoto(expr, 0) ->
 11;
yeccgoto(expr, 9) ->
 61;
yeccgoto(expr, 12) ->
 60;
yeccgoto(expr, 15) ->
 58;
yeccgoto(expr, 17) ->
 39;
yeccgoto(expr, 42) ->
 57;
yeccgoto(expr, 43) ->
 56;
yeccgoto(expr, 44) ->
 55;
yeccgoto(expr, 45) ->
 54;
yeccgoto(expr, 74) ->
 11;
yeccgoto(expr, 76) ->
 79;
yeccgoto(id, 0) ->
 10;
yeccgoto(id, 9) ->
 10;
yeccgoto(id, 12) ->
 10;
yeccgoto(id, 15) ->
 10;
yeccgoto(id, 17) ->
 10;
yeccgoto(id, 18) ->
 25;
yeccgoto(id, 24) ->
 25;
yeccgoto(id, 28) ->
 25;
yeccgoto(id, 42) ->
 10;
yeccgoto(id, 43) ->
 10;
yeccgoto(id, 44) ->
 10;
yeccgoto(id, 45) ->
 10;
yeccgoto(id, 62) ->
 66;
yeccgoto(id, 74) ->
 10;
yeccgoto(id, 76) ->
 10;
yeccgoto(mult_op, 11) ->
 44;
yeccgoto(mult_op, 39) ->
 44;
yeccgoto(mult_op, 54) ->
 44;
yeccgoto(mult_op, 55) ->
 44;
yeccgoto(mult_op, 56) ->
 44;
yeccgoto(mult_op, 57) ->
 44;
yeccgoto(mult_op, 58) ->
 44;
yeccgoto(mult_op, 60) ->
 44;
yeccgoto(mult_op, 61) ->
 44;
yeccgoto(mult_op, 79) ->
 44;
yeccgoto(path_op, 11) ->
 43;
yeccgoto(path_op, 39) ->
 43;
yeccgoto(path_op, 54) ->
 43;
yeccgoto(path_op, 55) ->
 43;
yeccgoto(path_op, 56) ->
 43;
yeccgoto(path_op, 57) ->
 43;
yeccgoto(path_op, 58) ->
 43;
yeccgoto(path_op, 60) ->
 43;
yeccgoto(path_op, 61) ->
 43;
yeccgoto(path_op, 79) ->
 43;
yeccgoto(prefix_op, 0) ->
 9;
yeccgoto(prefix_op, 9) ->
 9;
yeccgoto(prefix_op, 12) ->
 9;
yeccgoto(prefix_op, 15) ->
 9;
yeccgoto(prefix_op, 17) ->
 9;
yeccgoto(prefix_op, 42) ->
 9;
yeccgoto(prefix_op, 43) ->
 9;
yeccgoto(prefix_op, 44) ->
 9;
yeccgoto(prefix_op, 45) ->
 9;
yeccgoto(prefix_op, 74) ->
 9;
yeccgoto(prefix_op, 76) ->
 9;
yeccgoto(regatom, 0) ->
 8;
yeccgoto(regatom, 9) ->
 8;
yeccgoto(regatom, 12) ->
 8;
yeccgoto(regatom, 15) ->
 8;
yeccgoto(regatom, 17) ->
 8;
yeccgoto(regatom, 42) ->
 8;
yeccgoto(regatom, 43) ->
 8;
yeccgoto(regatom, 44) ->
 8;
yeccgoto(regatom, 45) ->
 8;
yeccgoto(regatom, 62) ->
 65;
yeccgoto(regatom, 74) ->
 8;
yeccgoto(regatom, 76) ->
 8;
yeccgoto(regexp, 0) ->
 7;
yeccgoto(regexp, 9) ->
 7;
yeccgoto(regexp, 12) ->
 7;
yeccgoto(regexp, 15) ->
 7;
yeccgoto(regexp, 17) ->
 7;
yeccgoto(regexp, 42) ->
 7;
yeccgoto(regexp, 43) ->
 7;
yeccgoto(regexp, 44) ->
 7;
yeccgoto(regexp, 45) ->
 7;
yeccgoto(regexp, 74) ->
 7;
yeccgoto(regexp, 76) ->
 7;
yeccgoto(regint, 67) ->
 70;
yeccgoto(regstr, 0) ->
 6;
yeccgoto(regstr, 9) ->
 6;
yeccgoto(regstr, 12) ->
 6;
yeccgoto(regstr, 15) ->
 6;
yeccgoto(regstr, 17) ->
 6;
yeccgoto(regstr, 42) ->
 6;
yeccgoto(regstr, 43) ->
 6;
yeccgoto(regstr, 44) ->
 6;
yeccgoto(regstr, 45) ->
 6;
yeccgoto(regstr, 62) ->
 64;
yeccgoto(regstr, 67) ->
 69;
yeccgoto(regstr, 74) ->
 6;
yeccgoto(regstr, 76) ->
 6;
yeccgoto(regvar, 0) ->
 5;
yeccgoto(regvar, 9) ->
 5;
yeccgoto(regvar, 12) ->
 5;
yeccgoto(regvar, 15) ->
 5;
yeccgoto(regvar, 17) ->
 5;
yeccgoto(regvar, 42) ->
 5;
yeccgoto(regvar, 43) ->
 5;
yeccgoto(regvar, 44) ->
 5;
yeccgoto(regvar, 45) ->
 5;
yeccgoto(regvar, 62) ->
 5;
yeccgoto(regvar, 67) ->
 68;
yeccgoto(regvar, 74) ->
 5;
yeccgoto(regvar, 76) ->
 5;
yeccgoto(restr_op, 11) ->
 42;
yeccgoto(restr_op, 39) ->
 42;
yeccgoto(restr_op, 54) ->
 42;
yeccgoto(restr_op, 55) ->
 42;
yeccgoto(restr_op, 56) ->
 42;
yeccgoto(restr_op, 57) ->
 42;
yeccgoto(restr_op, 58) ->
 42;
yeccgoto(restr_op, 60) ->
 42;
yeccgoto(restr_op, 61) ->
 42;
yeccgoto(restr_op, 79) ->
 42;
yeccgoto(statement, 0) ->
 4;
yeccgoto(statement, 74) ->
 4;
yeccgoto(statements, 0) ->
 3;
yeccgoto(statements, 74) ->
 75;
yeccgoto(type, 6) ->
 73;
yeccgoto(type, 13) ->
 59;
yeccgoto(type, 31) ->
 32;
yeccgoto(type, 36) ->
 37;
yeccgoto(type, 70) ->
 72;
yeccgoto(variable, 0) ->
 2;
yeccgoto(variable, 9) ->
 38;
yeccgoto(variable, 12) ->
 38;
yeccgoto(variable, 15) ->
 38;
yeccgoto(variable, 17) ->
 38;
yeccgoto(variable, 42) ->
 38;
yeccgoto(variable, 43) ->
 38;
yeccgoto(variable, 44) ->
 38;
yeccgoto(variable, 45) ->
 38;
yeccgoto(variable, 62) ->
 63;
yeccgoto(variable, 67) ->
 63;
yeccgoto(variable, 74) ->
 2;
yeccgoto(variable, 76) ->
 38;
yeccgoto(xref, 0) ->
 1;
yeccgoto(__Symbol, __State) ->
 erlang:error({yecc_bug,"1.1",{__Symbol, __State, missing_in_goto_table}}).

-compile({inline,{'yeccpars2_2_$end',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_2_$end'([__1 | __Stack]) ->
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,{'yeccpars2_2_*',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_2_*'([__1 | __Stack]) ->
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,{'yeccpars2_2_+',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_2_+'([__1 | __Stack]) ->
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,{'yeccpars2_2_-',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_2_-'([__1 | __Stack]) ->
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_2_of,1}}).
-file("xref_parser.yrl", 63).
yeccpars2_2_of([__1 | __Stack]) ->
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,{'yeccpars2_2_|',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_2_|'([__1 | __Stack]) ->
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,{'yeccpars2_2_||',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_2_||'([__1 | __Stack]) ->
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,{'yeccpars2_2_|||',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_2_|||'([__1 | __Stack]) ->
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_2_,1}}).
-file("xref_parser.yrl", 96).
yeccpars2_2_([__1 | __Stack]) ->
 [begin
   check_regexp_variable ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_4_,1}}).
-file("xref_parser.yrl", 54).
yeccpars2_4_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{'yeccpars2_6_$end',1}}).
-file("xref_parser.yrl", 102).
'yeccpars2_6_$end'(__Stack) ->
 [begin
   unknown
  end | __Stack].

-compile({inline,{'yeccpars2_6_)',1}}).
-file("xref_parser.yrl", 102).
'yeccpars2_6_)'(__Stack) ->
 [begin
   unknown
  end | __Stack].

-compile({inline,{'yeccpars2_6_*',1}}).
-file("xref_parser.yrl", 102).
'yeccpars2_6_*'(__Stack) ->
 [begin
   unknown
  end | __Stack].

-compile({inline,{'yeccpars2_6_+',1}}).
-file("xref_parser.yrl", 102).
'yeccpars2_6_+'(__Stack) ->
 [begin
   unknown
  end | __Stack].

-compile({inline,{'yeccpars2_6_,',1}}).
-file("xref_parser.yrl", 102).
'yeccpars2_6_,'(__Stack) ->
 [begin
   unknown
  end | __Stack].

-compile({inline,{'yeccpars2_6_-',1}}).
-file("xref_parser.yrl", 102).
'yeccpars2_6_-'(__Stack) ->
 [begin
   unknown
  end | __Stack].

-compile({inline,{yeccpars2_6_of,1}}).
-file("xref_parser.yrl", 102).
yeccpars2_6_of(__Stack) ->
 [begin
   unknown
  end | __Stack].

-compile({inline,{'yeccpars2_6_|',1}}).
-file("xref_parser.yrl", 102).
'yeccpars2_6_|'(__Stack) ->
 [begin
   unknown
  end | __Stack].

-compile({inline,{'yeccpars2_6_||',1}}).
-file("xref_parser.yrl", 102).
'yeccpars2_6_||'(__Stack) ->
 [begin
   unknown
  end | __Stack].

-compile({inline,{'yeccpars2_6_|||',1}}).
-file("xref_parser.yrl", 102).
'yeccpars2_6_|||'(__Stack) ->
 [begin
   unknown
  end | __Stack].

-compile({inline,{'yeccpars2_10_$end',1}}).
-file("xref_parser.yrl", 79).
'yeccpars2_10_$end'([__1 | __Stack]) ->
 [begin
   { constant , unknown , vertex , __1 }
  end | __Stack].

-compile({inline,{'yeccpars2_10_)',1}}).
-file("xref_parser.yrl", 79).
'yeccpars2_10_)'([__1 | __Stack]) ->
 [begin
   { constant , unknown , vertex , __1 }
  end | __Stack].

-compile({inline,{'yeccpars2_10_*',1}}).
-file("xref_parser.yrl", 79).
'yeccpars2_10_*'([__1 | __Stack]) ->
 [begin
   { constant , unknown , vertex , __1 }
  end | __Stack].

-compile({inline,{'yeccpars2_10_+',1}}).
-file("xref_parser.yrl", 79).
'yeccpars2_10_+'([__1 | __Stack]) ->
 [begin
   { constant , unknown , vertex , __1 }
  end | __Stack].

-compile({inline,{'yeccpars2_10_,',1}}).
-file("xref_parser.yrl", 79).
'yeccpars2_10_,'([__1 | __Stack]) ->
 [begin
   { constant , unknown , vertex , __1 }
  end | __Stack].

-compile({inline,{'yeccpars2_10_-',1}}).
-file("xref_parser.yrl", 79).
'yeccpars2_10_-'([__1 | __Stack]) ->
 [begin
   { constant , unknown , vertex , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_10_decl,1}}).
-file("xref_parser.yrl", 79).
yeccpars2_10_decl([__1 | __Stack]) ->
 [begin
   { constant , unknown , vertex , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_10_of,1}}).
-file("xref_parser.yrl", 79).
yeccpars2_10_of([__1 | __Stack]) ->
 [begin
   { constant , unknown , vertex , __1 }
  end | __Stack].

-compile({inline,{'yeccpars2_10_|',1}}).
-file("xref_parser.yrl", 79).
'yeccpars2_10_|'([__1 | __Stack]) ->
 [begin
   { constant , unknown , vertex , __1 }
  end | __Stack].

-compile({inline,{'yeccpars2_10_||',1}}).
-file("xref_parser.yrl", 79).
'yeccpars2_10_||'([__1 | __Stack]) ->
 [begin
   { constant , unknown , vertex , __1 }
  end | __Stack].

-compile({inline,{'yeccpars2_10_|||',1}}).
-file("xref_parser.yrl", 79).
'yeccpars2_10_|||'([__1 | __Stack]) ->
 [begin
   { constant , unknown , vertex , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_10_,1}}).
-file("xref_parser.yrl", 88).
yeccpars2_10_([__1 | __Stack]) ->
 [begin
   { atom , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_11_,1}}).
-file("xref_parser.yrl", 55).
yeccpars2_11_([__1 | __Stack]) ->
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,{yeccpars2_13_,1}}).
-file("xref_parser.yrl", 102).
yeccpars2_13_(__Stack) ->
 [begin
   unknown
  end | __Stack].

-compile({inline,{yeccpars2_16_,1}}).
-file("xref_parser.yrl", 38).
yeccpars2_16_([__1 | __Stack]) ->
 [begin
   '#'
  end | __Stack].

-compile({inline,{yeccpars2_19_,1}}).
-file("xref_parser.yrl", 98).
yeccpars2_19_([__1 | __Stack]) ->
 [begin
   value_of ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_20_,1}}).
-file("xref_parser.yrl", 80).
yeccpars2_20_([__1 | __Stack]) ->
 [begin
   value_of ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_21_,1}}).
-file("xref_parser.yrl", 95).
yeccpars2_21_([__1 | __Stack]) ->
 [begin
   check_regexp ( value_of ( __1 ) )
  end | __Stack].

-compile({inline,{yeccpars2_22_,1}}).
-file("xref_parser.yrl", 99).
yeccpars2_22_([__1 | __Stack]) ->
 [begin
   value_of ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_23_,1}}).
-file("xref_parser.yrl", 81).
yeccpars2_23_([__1 | __Stack]) ->
 [begin
   value_of ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_25_,1}}).
-file("xref_parser.yrl", 79).
yeccpars2_25_([__1 | __Stack]) ->
 [begin
   { constant , unknown , vertex , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_26_,1}}).
-file("xref_parser.yrl", 74).
yeccpars2_26_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_29_,1}}).
-file("xref_parser.yrl", 74).
yeccpars2_29_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_30_,1}}).
-file("xref_parser.yrl", 75).
yeccpars2_30_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __2 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_31_,1}}).
-file("xref_parser.yrl", 102).
yeccpars2_31_(__Stack) ->
 [begin
   unknown
  end | __Stack].

-compile({inline,{yeccpars2_32_,1}}).
-file("xref_parser.yrl", 61).
yeccpars2_32_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   type ( { tuple , [ __2 | __3 ] } , __5 )
  end | __Stack].

-compile({inline,{yeccpars2_33_,1}}).
-file("xref_parser.yrl", 101).
yeccpars2_33_([__1 | __Stack]) ->
 [begin
   value_of ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_34_,1}}).
-file("xref_parser.yrl", 74).
yeccpars2_34_(__Stack) ->
 [begin
   [ ]
  end | __Stack].

-compile({inline,{yeccpars2_36_,1}}).
-file("xref_parser.yrl", 102).
yeccpars2_36_(__Stack) ->
 [begin
   unknown
  end | __Stack].

-compile({inline,{yeccpars2_37_,1}}).
-file("xref_parser.yrl", 60).
yeccpars2_37_([__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   type ( { list , [ __2 | __3 ] } , __5 )
  end | __Stack].

-compile({inline,{'yeccpars2_38_$end',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_38_$end'([__1 | __Stack]) ->
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,{'yeccpars2_38_)',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_38_)'([__1 | __Stack]) ->
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,{'yeccpars2_38_*',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_38_*'([__1 | __Stack]) ->
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,{'yeccpars2_38_+',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_38_+'([__1 | __Stack]) ->
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,{'yeccpars2_38_,',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_38_,'([__1 | __Stack]) ->
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,{'yeccpars2_38_-',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_38_-'([__1 | __Stack]) ->
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_38_of,1}}).
-file("xref_parser.yrl", 63).
yeccpars2_38_of([__1 | __Stack]) ->
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,{'yeccpars2_38_|',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_38_|'([__1 | __Stack]) ->
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,{'yeccpars2_38_||',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_38_||'([__1 | __Stack]) ->
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,{'yeccpars2_38_|||',1}}).
-file("xref_parser.yrl", 63).
'yeccpars2_38_|||'([__1 | __Stack]) ->
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_38_,1}}).
-file("xref_parser.yrl", 96).
yeccpars2_38_([__1 | __Stack]) ->
 [begin
   check_regexp_variable ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_41_,1}}).
-file("xref_parser.yrl", 43).
yeccpars2_41_([__3,__2,__1 | __Stack]) ->
 [begin
   value_of ( __2 )
  end | __Stack].

-compile({inline,{yeccpars2_46_,1}}).
-file("xref_parser.yrl", 72).
yeccpars2_46_([__3,__2,__1 | __Stack]) ->
 [begin
   __2
  end | __Stack].

-compile({inline,{yeccpars2_47_,1}}).
-file("xref_parser.yrl", 37).
yeccpars2_47_([__1 | __Stack]) ->
 [begin
   intersection
  end | __Stack].

-compile({inline,{yeccpars2_48_,1}}).
-file("xref_parser.yrl", 35).
yeccpars2_48_([__1 | __Stack]) ->
 [begin
   union
  end | __Stack].

-compile({inline,{yeccpars2_49_,1}}).
-file("xref_parser.yrl", 36).
yeccpars2_49_([__1 | __Stack]) ->
 [begin
   difference
  end | __Stack].

-compile({inline,{yeccpars2_50_,1}}).
-file("xref_parser.yrl", 42).
yeccpars2_50_([__1 | __Stack]) ->
 [begin
   'of'
  end | __Stack].

-compile({inline,{yeccpars2_51_,1}}).
-file("xref_parser.yrl", 39).
yeccpars2_51_([__1 | __Stack]) ->
 [begin
   '|'
  end | __Stack].

-compile({inline,{yeccpars2_52_,1}}).
-file("xref_parser.yrl", 40).
yeccpars2_52_([__1 | __Stack]) ->
 [begin
   '||'
  end | __Stack].

-compile({inline,{yeccpars2_53_,1}}).
-file("xref_parser.yrl", 41).
yeccpars2_53_([__1 | __Stack]) ->
 [begin
   '|||'
  end | __Stack].

-compile({inline,{yeccpars2_54_,1}}).
-file("xref_parser.yrl", 64).
yeccpars2_54_([__3,__2,__1 | __Stack]) ->
 [begin
   { set , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_55_,1}}).
-file("xref_parser.yrl", 65).
yeccpars2_55_([__3,__2,__1 | __Stack]) ->
 [begin
   { set , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_56_,1}}).
-file("xref_parser.yrl", 68).
yeccpars2_56_([__3,__2,__1 | __Stack]) ->
 [begin
   { path , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_57_,1}}).
-file("xref_parser.yrl", 67).
yeccpars2_57_([__3,__2,__1 | __Stack]) ->
 [begin
   { restr , __2 , __1 , __3 }
  end | __Stack].

-compile({inline,{yeccpars2_58_,1}}).
-file("xref_parser.yrl", 69).
yeccpars2_58_([__2,__1 | __Stack]) ->
 [begin
   { type , { convert , __1 } , __2 }
  end | __Stack].

-compile({inline,{yeccpars2_59_,1}}).
-file("xref_parser.yrl", 62).
yeccpars2_59_([__2,__1 | __Stack]) ->
 [begin
   type ( __1 , __2 )
  end | __Stack].

-compile({inline,{yeccpars2_60_,1}}).
-file("xref_parser.yrl", 66).
yeccpars2_60_([__2,__1 | __Stack]) ->
 [begin
   prefix ( __1 , __2 )
  end | __Stack].

-compile({inline,{yeccpars2_61_,1}}).
-file("xref_parser.yrl", 70).
yeccpars2_61_([__2,__1 | __Stack]) ->
 [begin
   prefix ( __1 , __2 )
  end | __Stack].

-compile({inline,{yeccpars2_63_,1}}).
-file("xref_parser.yrl", 96).
yeccpars2_63_([__1 | __Stack]) ->
 [begin
   check_regexp_variable ( __1 )
  end | __Stack].

-compile({inline,{yeccpars2_66_,1}}).
-file("xref_parser.yrl", 88).
yeccpars2_66_([__1 | __Stack]) ->
 [begin
   { atom , __1 }
  end | __Stack].

-compile({inline,{yeccpars2_70_,1}}).
-file("xref_parser.yrl", 102).
yeccpars2_70_(__Stack) ->
 [begin
   unknown
  end | __Stack].

-compile({inline,{yeccpars2_71_,1}}).
-file("xref_parser.yrl", 92).
yeccpars2_71_([__1 | __Stack]) ->
 [begin
   { integer , value_of ( __1 ) }
  end | __Stack].

-compile({inline,{yeccpars2_72_,1}}).
-file("xref_parser.yrl", 85).
yeccpars2_72_([__6,__5,__4,__3,__2,__1 | __Stack]) ->
 [begin
   regexp ( func , { __1 , __3 , __5 } , __6 )
  end | __Stack].

-compile({inline,{yeccpars2_73_,1}}).
-file("xref_parser.yrl", 83).
yeccpars2_73_([__2,__1 | __Stack]) ->
 [begin
   regexp ( atom , __1 , __2 )
  end | __Stack].

-compile({inline,{yeccpars2_75_,1}}).
-file("xref_parser.yrl", 56).
yeccpars2_75_([__3,__2,__1 | __Stack]) ->
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,{yeccpars2_77_,1}}).
-file("xref_parser.yrl", 34).
yeccpars2_77_([__1 | __Stack]) ->
 [begin
   user
  end | __Stack].

-compile({inline,{yeccpars2_78_,1}}).
-file("xref_parser.yrl", 33).
yeccpars2_78_([__1 | __Stack]) ->
 [begin
   tmp
  end | __Stack].

-compile({inline,{yeccpars2_79_,1}}).
-file("xref_parser.yrl", 58).
yeccpars2_79_([__3,__2,__1 | __Stack]) ->
 [begin
   { assign , __2 , __1 , __3 }
  end | __Stack].


-file("xref_parser.yrl", 304).
