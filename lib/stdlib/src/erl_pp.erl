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
%%     $Id$
%%
-module(erl_pp).

%% A rather simplistic pretty printer for Erlang code in the same
%% format as returned from the parser. It does not try and work out
%% when it should break a line, but breaks or does break according to
%% some predefined rules whatever the consequences. It should really
%% be smarter.

%% The after hook has been extended to work at each level, attribute,
%% function, clause, and expression. Lists of things, like guards, still
%% have to be lists of things.

-export([form/1,form/2,
	 attribute/1,attribute/2,function/1,function/2,rule/1,rule/2,
	 guard/1,guard/2,exprs/1,exprs/2,exprs/3,expr/1,expr/2,expr/3,expr/4]).

%% The following exports are here for backwards compatibility.
-export([seq/1,seq/2]).
-deprecated([{seq,1},{seq,2}]).

-import(lists, [flatten/1]).
-import(io_lib, [write/1,format/2,write_char/1,write_string/1,indentation/2]).
-import(erl_parse, [inop_prec/1,preop_prec/1,func_prec/0]).

%% seq(Expressions)
%% seq(Expressions, Hook)
%%  These calls are here for backwards compatibility (BC sucks!).

seq(Es) -> exprs(Es).

seq(Es, Hook) -> exprs(Es, Hook).

%% After each "thing" has been printed the cursor is left in place. It is
%% up to what comes after, or the caller to decide how to terminate the
%% previous "thing".

form(Thing) ->
     form(Thing, none).

form({attribute,Line,Name,Arg}, Hook) ->
    attribute({attribute,Line,Name,Arg}, Hook);
form({function,Line,Name,Arity,Clauses}, Hook) ->
    function({function,Line,Name,Arity,Clauses}, Hook);
form({rule,Line,Name,Arity,Clauses}, Hook) ->
    rule({rule,Line,Name,Arity,Clauses}, Hook);
%% These are specials to make it easier for the compiler.
form({error,E}, _Hook) ->
    format("~p~n", [{error,E}]);
form({warning,W}, _Hook) ->
    format("~p~n", [{warning,W}]);
form({eof,_Line}, _Hook) -> "\n".

attribute(Thing) ->
    attribute(Thing, none).

attribute({attribute,_Line,Name,Arg}, Hook) ->
    attribute(Name, Arg, Hook).

attribute(export, Falist, _Hook) ->
    format("-export(~s).\n", [falist(Falist)]);
attribute(import, Name, _Hook) when list(Name) ->
    format("-import(~s).\n", [pname(Name)]);
attribute(import, {From,Falist}, _Hook) when list(From) ->
    format("-import(~s, ~s).\n", [pname(From),falist(Falist)]);
attribute(import, {From,Falist}, _Hook) ->
    format("-import(~w, ~s).\n", [From,falist(Falist)]);
attribute(file, {Name,Line}, _Hook) ->
    format("-file(~p, ~w).\n", [Name,Line]);
attribute(record, {Name,Is}, Hook) ->
    Nl = flatten(write(Name)),
    format("-record(~w, ~s).\n",
	   [Name,record_fields(Is, 10+length(Nl), Hook)]);
attribute(module, Name, _Hook) when list(Name) ->
    format("-module(~s).\n", [pname(Name)]);
attribute(Name, Arg, _Hook) ->
    format("-~w(~w).\n", [Name,Arg]).

pname(['' | As]) ->
    [$. | pname(As)];
pname([A]) -> write(A);
pname([A | As]) ->
    [write(A),$.|pname(As)].

falist([]) -> "[]";
falist(Fas) -> falist(Fas, $[).

falist([{Name,Arity}|Falist], P) ->
    [P,write(Name),$/,write(Arity)|falist(Falist, $,)];
falist([], _) -> [$]].

function(F) -> function(F, none).

function({function,_Line,Name,_Arity,Cs}, Hook) ->
    [clauses(fun (C, _, H) -> func_clause(Name, C, H) end, 0, Hook, Cs),".\n"].

func_clause(Name, {clause,Line,Head,Guard,Body}, Hook) ->
    [expr({call,Line,{atom,Line,Name},Head}, 0, Hook),
     guard(Guard, 0, Hook),
     body(Body, 4, Hook)].

rule(R) -> rule(R, none).

rule({rule,_Line,Name,_Arity,Cs}, Hook) ->
    [clauses(fun (C, _, H) -> rule_clause(Name, C, H) end, 0, Hook, Cs),".\n"].

rule_clause(Name, {clause,Line,Head,Guard,Body}, Hook) ->
    [expr({call,Line,{atom,Line,Name},Head}, 0, Hook),
     guard(Guard, 0, Hook),
     rule_body(Body, 4, Hook)].

rule_body(Es, I, Hook) ->
     [" :-",nl_indent(I)|lc_quals(Es, I, Hook)].

%% guard(GuardExpressions)
%% guard(GuardExpressions, Hook)
%% guard(GuardExpressions, Indentation, Hook)

guard(Gs) -> guard(Gs, none).

guard(Gs, Hook) -> guard(Gs, 0, Hook).

guard_no_when([E|Es], I, Hook) when list(E) ->
    separated_list(fun guard0/3, "; ", I, Hook, [E|Es]);
guard_no_when([E|Es], I, Hook) ->
    guard_no_when([[E|Es]], I, Hook);
guard_no_when([], _, _) -> [].

guard([E|Es], I, Hook) when list(E) ->
    " when " ++ separated_list(fun guard0/3, "; ", I, Hook, [E|Es]);
guard([E|Es], I, Hook) ->
    guard([[E|Es]], I, Hook);
guard([], _, _) -> [].

guard0([E|Es], I, Hook) ->
    separated_list(fun expr/3, ", ", I, Hook, [E|Es]);
guard0([], _, _) -> [].

%% body(Es, Indentation, Hook) -> [Char].

body(Es, I, Hook) ->
    [" ->",nl_indent(I)|exprs(Es, I, Hook)].

%% exprs(Expressions)
%% exprs(Expressions, Hook)
%% exprs(Expressions, Indentation, Punctuation, Hook)
%%  Prettyprint expressions.

exprs(Es) ->
    exprs(Es, none).

exprs(Es, Hook) ->
    exprs(Es, -10000, Hook).			%A hack to prohibit line breaks

exprs(Es, I, Hook) ->
    separated_list(fun expr/3, "," ++ nl_indent(I), I, Hook, Es).

%% expr(Expression)
%% expr(Expression, Hook)
%% expr(Expression, Indentation, Hook)
%% expr(Expression, Indentation, Precedence, Hook)
%%  Prettyprint one expr. Seeing everything is a "expr" we have to handle
%%  operator precedences for arithmetic expressions as well.
%%  N.B. We use a simple length/1 call to calculate indent

expr(E) -> expr(E, -10000, 0, none).

expr(E, Hook) -> expr(E, -10000, 0, Hook).

expr(E, I, Hook) -> expr(E, I, 0, Hook).

expr({var,_,V}, _, _, _) when integer(V) ->	%Special hack for Robert
    format("_~w", [V]);
expr({var,_,V}, _, _, _) -> format("~s", [V]);
expr({char,_,C}, _, _, _) -> write_char(C);	%When this comes
expr({integer,_,N}, _, _, _) -> write(N);
expr({float,_,F}, _, _, _) -> write(F);
expr({atom,_,A}, _, _, _) -> write(A);
expr({string,_,S}, _, _, _) -> write_string(S);
expr({nil,_}, _, _, _) -> "[]";
expr({cons,_,H,T}, I, _, Hook) ->
    I1 = I + 1,
    [$[,expr(H, I1, 0, Hook)|tail(T, I1, Hook)];
expr({lc,_,E,Qs}, I, _Prec, Hook) ->
    ["[ ",
     expr(E, I+2, 0, Hook),
     " ||", nl_indent(I+4),
     lc_quals(Qs, I+4, Hook),
     nl_indent(I),"]"];
expr({tuple,_,Elts}, I, _, Hook) ->
    expr_list(Elts, "{", "}", I, Hook);
%%expr({struct,_,Tag,Elts}, I, _, Hook) ->
%%    Tl = flatten(write(Tag)),
%%    [Tl|expr_list(Elts, "{", "}", I+length(Tl), Hook)];
expr({record_index, _, Name, F}, I, Prec, Hook) ->
    {P,R} = preop_prec('#'),
    Nl = flatten(write(Name)),
    El = ["#",Nl,".",expr(F, I+length(Nl)+2, R, Hook)],
    maybe_paren(P, Prec, El);
expr({record, _, Name, Fs}, I, Prec, Hook) ->
    {P,_R} = preop_prec('#'),
    Nl = flatten(write(Name)),
    El = ["#",Nl,record_fields(Fs, I+length(Nl)+1, Hook)],
    maybe_paren(P, Prec, El);
expr({record_field, _, Rec, Name, F}, I, Prec, Hook) ->
    {L,P,R} = inop_prec('#'),
    Rl = expr(Rec, I, L, Hook),
    Nl = flatten(write(Name)),
    El = [Rl,"#",Nl,".",expr(F, indentation(Rl, I)+length(Nl)+2, R, Hook)],
    maybe_paren(P, Prec, El);
expr({record, _, Rec, Name, Fs}, I, Prec, Hook) ->
    {L,P,_R} = inop_prec('#'),
    Rl = expr(Rec, I, L, Hook),
    Nl = flatten(write(Name)),
    El = [Rl,"#",Nl,record_fields(Fs, indentation(Rl, I)+length(Nl)+1, Hook)],
    maybe_paren(P, Prec, El);
expr({record_field, _, {atom,_,''}, F}, I, Prec, Hook) ->
    {_,P,R} = inop_prec('.'),
    El = [".",expr(F, I + 1, R, Hook)],
    maybe_paren(P, Prec, El);
expr({record_field, _, Rec, F}, I, Prec, Hook) ->
    {L,P,R} = inop_prec('.'),
    Rl = expr(Rec, I, L, Hook),
    El = [Rl,".",expr(F, indentation(Rl, I)+1, R, Hook)],
    maybe_paren(P, Prec, El);
expr({block,_,Es}, I, _, Hook) ->
    ["begin",nl_indent(I+4),
     exprs(Es, I+4, Hook),
     nl_indent(I), "end"];
expr({'if',_,Cs}, I, _, Hook) ->
    I1 = I + 4,
    ["if",nl_indent(I1),
     if_clauses(Cs, I1, Hook),
     nl_indent(I),"end"];
expr({'case',_,Expr,Cs}, I, _, Hook) ->
    I1 = I + 4,
    ["case ",
     expr(Expr, I+5, 0, Hook),
     " of",nl_indent(I1),
     cr_clauses(Cs, I1, Hook),
     nl_indent(I),"end"];
expr({'receive',_,Cs}, I, _, Hook) ->
    I1 = I + 4,
    ["receive",nl_indent(I1),
     cr_clauses(Cs, I1, Hook),
     nl_indent(I),"end"];
expr({'receive',_,Cs,To,ToOpt}, I, _, Hook) ->
    I1 = I + 4,
    ["receive",nl_indent(I1),
     if						%Must special case no clauses
	 Cs /= [] -> cr_clauses(Cs, I1, Hook);
	 true -> ""
     end,
     %% Now for the timeout bit.
     nl_indent(I), "after",
     nl_indent(I1),
     expr(To, I1, 0, Hook),
     body(ToOpt, I1+4, Hook),
     nl_indent(I), "end"];
expr({'fun',_,{function,F,A}}, _I, _Prec, _Hook) ->
    ["fun ",write(F),$/,write(A)];
expr({'fun',_,{clauses,Cs}}, I, _Prec, Hook) ->
    ["fun ",
     fun_clauses(Cs, I+4, Hook),
     " end"];
expr({'fun',_,{clauses,Cs},Extra}, I, _Prec, Hook) ->
    [io_lib:format("% fun-info: ~p~n", [Extra]),
     string:chars($\s, I),
     "fun ",
     fun_clauses(Cs, I+4, Hook),
     " end"];
expr({'query',_,Lc}, I, _Prec, Hook) ->
    ["query ",
     expr(Lc, I+6, 0, Hook),
     " end"];
expr({call,_,Name,Args}, I, Prec, Hook) ->
    {F,P} = func_prec(),
    Nl = expr(Name, I, F, Hook),
    El = [Nl|expr_list(Args, "(", ")", indentation(Nl, I), Hook)],
    maybe_paren(P, Prec, El);
expr({'try',_,Es,Scs,Ccs}, I, _, Hook) ->
    I1 = I + 4,
    ["try",nl_indent(I1),
     exprs(Es, I1, Hook),
     nl_indent(I),
     if Scs == [] ->
	     [];
	true ->
	     ["of",nl_indent(I1),
	      cr_clauses(Scs, I1, Hook),nl_indent(I)]
     end,
     "catch",nl_indent(I1),
     cr_clauses(Ccs, I1, Hook),nl_indent(I),
     "end"];
expr({'catch',_,Expr}, I, Prec, Hook) ->
    {P,R} = preop_prec('catch'),
    El = ["catch ",expr(Expr, I+6, R, Hook)],
    maybe_paren(P, Prec, El);
expr({match,_,Lhs,Rhs}, I, Prec, Hook) ->
    {L,P,R} = inop_prec('='),
    Pl = expr(Lhs, I, L, Hook),
    El = [Pl," = ",expr(Rhs, indentation(Pl, I)+3, R, Hook)],
    maybe_paren(P, Prec, El);
expr({op,_,Op,Arg}, I, Prec, Hook) ->
    {P,R} = preop_prec(Op),
    Ol = flatten(format("~s ", [Op])),
    El = [Ol,expr(Arg, I+length(Ol), R, Hook)],
    maybe_paren(P, Prec, El);
expr({op,_,Op,Larg,Rarg}, I, Prec, Hook) ->
    {L,P,R} = inop_prec(Op),
    Ll = expr(Larg, I, L, Hook),
    Ol = flatten(format(" ~s ", [Op])),
    El = [Ll,Ol,expr(Rarg, indentation(Ll, I)+length(Ol), R, Hook)],
    maybe_paren(P, Prec, El);
%% Special expressions which are not really legal everywhere.
expr({remote,_,M,F}, I, Prec, Hook) ->
    {L,P,R} = inop_prec(':'),
    Ml = expr(M, I, L, Hook),
    El = [Ml,":",expr(F, indentation(Ml, I)+1, R, Hook)],
    maybe_paren(P, Prec, El);
%% BIT SYNTAX:
expr({bin,_,Fs}, I, _, Hook) ->
    bit_grp(Fs,I,Hook);
%% Special case for straight values.
expr({value,_,Val}, _, _,_) ->
    write(Val);
%% Now do the hook.
expr(Other, _Indentation, _Precedence, none) ->
    format("INVALID-FORM:~w:",[Other]);
expr(Expr, Indentation, Precedence, {Mod,Func,Eas}) when Mod /= 'fun' ->
    apply(Mod, Func, [Expr,Indentation,Precedence,{Mod,Func,Eas}|Eas]);
expr(Expr, Indentation, Precedence, Func) ->
    Func(Expr, Indentation, Precedence, Func).

%% BITS:
bit_grp(Fs,I,Hook) ->
    ["<<", bit_elems(Fs,I+2,Hook),">>"].

bit_elems([E], I, Hook) ->
    [ bit_elem(E, I, Hook) ];
bit_elems([E|Es], I, Hook) ->
    [ bit_elem(E, I, Hook), ",", nl_indent(I),
      bit_elems(Es, I, Hook) ];
bit_elems([],_I,_Hook) ->
    [].

bit_elem({bin_element,_,Expr,Sz,Types}, I, Hook) ->
    Expr1 = 
	if Sz =/= default ->
		{remote, 0, Expr, Sz};
	   true ->
		Expr
	end,
    if Types =/= default ->
	    [expr(Expr1,I,0,Hook), $/, bit_elem_types(Types)];
       true ->
	    expr(Expr1,I,0,Hook)
    end.

bit_elem_types([T]) ->
    bit_elem_type(T);
bit_elem_types([T | Rest]) ->
    [bit_elem_type(T), $-, bit_elem_types(Rest)].

bit_elem_type({A, B}) ->
    expr({remote, 0, erl_parse:abstract(A), erl_parse:abstract(B)});
bit_elem_type(T) ->
    expr(erl_parse:abstract(T)).

%%% end of BITS

record_fields(Fs, I, Hook) ->
    I1 = I + 1,
    ["{",
     separated_list(fun record_field/3, "," ++ nl_indent(I1), I1, Hook, Fs),
     "}"].

record_field({record_field,_,F,Val}, I, Hook) ->
    {L,_P,R} = inop_prec('='),
    Fl = expr(F, I, L, Hook),
    [Fl," = ",expr(Val, indentation(Fl, I)+3, R, Hook)];
record_field({record_field,_,F}, I, Hook) ->
    expr(F, I, Hook).

tail({cons,_,H,T}, I, Hook) ->
    [$,, expr(H, I, 0, Hook)|tail(T, I, Hook)];
tail({nil,_}, _, _) -> "]";
tail(Other, I, Hook) ->
    [$|,expr(Other, I, 0, Hook),$]].

expr_list(Es, First, Last, I, Hook) ->
    [First,
     separated_list(fun expr/3, ",", I+length(First), Hook, Es),
     Last].

%% if_clauses(Clauses, Indentation, Hook) -> [Char].
%%  Print 'if' clauses.

if_clauses(Cs, I, Hook) -> clauses(fun if_clause/3, I, Hook, Cs).

if_clause({clause,_,[],G,B}, I, Hook) ->
    [if
	 G /= [] -> guard_no_when(G, I+2, Hook);
	 true -> write(true)
     end,
     body(B, I+4, Hook)].

%% cr_clauses(Clauses, Indentation, Hook) -> [Char].
%%  Print 'case'/'receive' clauses.

cr_clauses(Cs, I, Hook) -> clauses(fun cr_clause/3, I, Hook, Cs).

cr_clause({clause,_,[T],G,B}, I, Hook) ->
    [expr(T, I, 0, Hook),
     guard(G, I, Hook),
     body(B, I+4, Hook)].

%% fun_clauses(Clauses, Indentation, Hook) -> [Char].
%%  Print 'fun' clauses.

fun_clauses(Cs, I, Hook) -> clauses(fun fun_clause/3, I, Hook, Cs).

fun_clause({clause,_,A,G,B}, I, Hook) ->
    [expr_list(A, "(", ")", I, Hook),
     guard(G, I, Hook),
     body(B, I+4, Hook)].

%% clauses(Type, Identation, Hook) -> [Char].
%%  Generic clause printing function.

clauses(Type, I, Hook, Cs) ->
    separated_list(Type, ";" ++ nl_indent(I), I, Hook, Cs).

%% separated_list(Fun, Sep, Indentation, Hook, Es) -> [Char].
%%  Generic function for printing a list of things with separators
%%  between them. We can handle the empty case.

separated_list(Fun, S, I, Hook, [E1|Es]) ->
    [Fun(E1, I, Hook)|lists:map( fun (E) -> [S,Fun(E, I, Hook)] end, Es)];
separated_list(_Fun, _S, _I, _Hook, []) -> "".

%% lc_quals(Qualifiers, Indentation, Hook)
%% List comprehension qualifiers

lc_quals(Qs, I, Hook) ->
    separated_list(fun lc_qual/3, "," ++ nl_indent(I), I, Hook, Qs).

lc_qual({generate,_,Pat,E}, I, Hook) ->
    Pl = expr(Pat, I, 0, Hook),
    [Pl," <- ",expr(E, indentation(Pl, I)+4, 0, Hook)];
lc_qual(Q, I, Hook) ->
    expr(Q, I, 0, Hook).

%%
%% Utilities
%%

maybe_paren(P, Prec, Expr) when P < Prec -> [$(,Expr,$)];
maybe_paren(_P, _Prec, Expr) -> Expr.

nl_indent(I) when I >= 0 -> [$\n|string:chars($\s, I)];
nl_indent(_) -> " ".
