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
%% Purpose : Kernel Erlang optimiser.

-module(v2_kernopt).
-export([module/2]).

-import(lists, [map/2,mapfoldl/3]).

-record(dummy, {}).

module({Mod,Exp,Attr,Forms}, Options) ->
    Mfs = map(fun function/1, Forms),
    {ok, {Mod,Exp,Attr,Mfs}}.

function({function,Name,Arity,Cs0}) ->
    St0 = #dummy{},
    {Cs1,St1} = mapfoldl(fun (C, St) -> clause(C, St) end, St0, Cs0),
    {function,Name,Arity,Cs1};
function({asm,Name,Arity,Code}=Asm) ->
    Asm.

clause({clause,H,G,B0}, St0) ->
    {B1,St1} = exprs(B0, St0),
    {{clause,H,G,B1}, St1}.

exprs([{set,V,E}|Kes0], St0) ->
    %% Constant folding.
    case simple(E) of
	true -> exprs(subst_var(Kes0, V, E), St0);
	false -> 
	    {Kes1,St1} = exprs(Kes0, St0),
	    {[{set,V,E}|Kes1],St1}
    end;
exprs([{bif,Name,As,[Dst]}|Kes0], St0) ->
    %% Evaluation of constant expressions.
    case eval_bif(Name, As) of
	{value,Value} -> exprs([{set,Dst,Value}|Kes0], St0);
	error ->
	    {Kes1,St1} = exprs(Kes0, St0),
	    {[{bif,Name,As,[Dst]}|Kes1],St1}
    end;
exprs([{call,Name,As,[Dst]}|Kes0], St0) ->
    %% Evaluation of constant expressions.
    case eval_bif(Name, As) of
	{value,Value} -> exprs([{set,Dst,Value}|Kes0], St0);
	error ->
	    {Kes1,St1} = exprs(Kes0, St0),
	    {[{call,Name,As,[Dst]}|Kes1],St1}
    end;
exprs([{enter,Name,As}|Kes0], St0) ->
    %% Evaluation of constant expressions.
    case eval_bif(Name, As) of
	{value,Value} -> exprs([{return,[Value]}|Kes0], St0);
	error ->
	    {Kes1,St1} = exprs(Kes0, St0),
	    {[{enter,Name,As}|Kes1],St1}
    end;
exprs([Ke0|Kes0], St0) ->
    {Ke1,St1} = expr(Ke0, St0),
    {Kes1,St2} = exprs(Kes0, St1),
    {[Ke1|Kes1],St2};
exprs([], St) -> {[],St}.

expr({'case',Cvar,Cs0,Rs}, St0) ->
    {Cs1,St1} = mapfoldl(fun (C, St) -> cclause(Cvar, C, St) end, St0, Cs0),
    {{'case',Cvar,Cs1,Rs}, St0};
expr({'if',Cs0,Rs}, St0) ->
    {Cs1,St1} = mapfoldl(fun (C, St) -> clause(C, St) end, St0, Cs0),
    {{'if',Cs1,Rs}, St1};
expr({receive_loop,Te,Cs0,To0,Rs}, St0) ->
    {Cs1,St1} = mapfoldl(fun (C, St) -> clause(C, St) end, St0, Cs0),
    {To1,St2} = exprs(To0, St1),
    {{receive_loop,Te,Cs1,To1,Rs},St2};
expr(E, St) -> {E, St}.

cclause(Cvar, {clause,[{var,V}],G0,B0}, St0) ->
    G1 = subst_var(G0, {var,V}, Cvar),
    B1 = subst_var(B0, {var,V}, Cvar),
    {G2,St1} = exprs(G1, St0),
    {B2,St2} = exprs(B1, St1),
    {{clause,[Cvar],G2,B2}, St2};
cclause(Cvar, {clause,H0,G0,B0}, St0) ->
    {G1,St1} = exprs(G0, St0),
    {B1,St2} = exprs(B0, St1),
    {{clause,H0,G1,B1}, St2}.

eval_bif({remote,erlang,Name}, [A]) ->
    eval_result(catch op(Name, value(A)));
eval_bif(Name, [A]) ->
    eval_result(catch op(Name, value(A)));
eval_bif({remote,erlang,Name}, [A1,A2]) ->
    eval_result(catch op(Name, value(A1), value(A2)));
eval_bif(Name, [A1,A2]) ->
    eval_result(catch op(Name, value(A1), value(A2)));
eval_bif({remote,erlang,Name}, [A1,A2,A3]) ->
    eval_result(catch op(Name, value(A1), value(A2), value(A3)));
eval_bif(Name, [A1,A2,A3]) ->
    eval_result(catch op(Name, value(A1), value(A2), value(A3)));
eval_bif(Name, As) -> error.

eval_result(X) when integer(X) -> {value, {integer,X}};
eval_result(X) when float(X)   -> {value, {float,X}};
eval_result(X) when atom(X)    -> {value, {atom,X}};
eval_result(X) when list(X)    -> {value, unvalue(X)};
eval_result(X) -> error.

value(nil) -> [];
value({integer,X}) -> X;
value({string,X}) -> X;
value({float,X})   -> X;
value({atom,X})    -> X;
value({tuple,Es}) -> list_to_tuple(map(fun(X) -> value(X) end, Es));
value({cons,_,H,T}) -> [value(H)|value(T)].

unvalue(V) ->
    fpattern(erl_parse:abstract(V)).

%% fpattern(P) -> Kpat.
%% Transform a pattern by removing line numbers.

fpattern({var,L,V}) -> {var,V};
fpattern({integer,L,I}) -> {integer,I};
fpattern({float,L,F}) -> {float,F};
fpattern({atom,L,A}) -> {atom,A};
fpattern({string,L,S}) -> {string,S};
fpattern({nil,L}) -> nil;
fpattern({cons,L,H,T}) ->
    {cons,[fpattern(H),fpattern(T)]};
fpattern({tuple,L,Ps}) ->
    {tuple,fpattern_list(Ps)}.

%% fpattern_list([P]) -> [P].

fpattern_list(Ps) -> map(fun fpattern/1, Ps).

op('+', X) -> 0 + X;
op('-', X) -> 0 - X;
op('bnot', X) -> bnot X;
op('not', X) ->  not X;
op(abs, A) -> abs(A);
op(float, A) -> float(A);
op(hd, A) -> hd(A);
op(length, A) -> length(A);
op(round, A) -> round(A);
op(tl, A) -> tl(A);
op(size, A) -> size(A);
op(trunc, A) -> trunc(A);
op(atom_to_list, A) -> atom_to_list(A);
op(float_to_list, A) -> float_to_list(A);
op(integer_to_list,A) -> integer_to_list(A);
op(list_to_atom,A) -> list_to_atom(A);
op(list_to_float,A) -> list_to_float(A);
op(list_to_integer,A) -> list_to_integer(A);
op(list_to_tuple,A) -> list_to_tuple(A);
op(tuple_to_list,A) -> tuple_to_list(A).

op(element, X, Y) -> element(X, Y);
op('*', X, Y) -> X * Y;
op('/', X, Y) -> X / Y;
op('+', X, Y) -> X + Y;
op('-', X, Y) -> X - Y;
op('div', X, Y) -> X div Y;
op('rem', X, Y) -> X rem Y;
op('band', X, Y) -> X band Y;
op('bor', X, Y) -> X bor Y;
op('bxor', X, Y) -> X bxor Y;
op('bsl', X, Y) -> X bsl Y;
op('bsr', X, Y) -> X bsr Y;
op('and', X, Y) -> X and Y;
op('or',  X, Y) -> X or Y;
op('xor', X, Y) -> X xor Y;
op('==',  X, Y) -> X == Y;
op('/=',  X, Y) -> X /= Y;
op('=<',  X, Y) -> X =< Y;
op('<',   X, Y) -> X < Y;
op('>=',  X, Y) -> X >= Y;
op('>',   X, Y) -> X > Y;
op('=:=', X, Y) -> X =:= Y;
op('=/=', X, Y) -> X =/= Y;
op('++', X, Y) -> X ++ Y;
op('--', X, Y) -> X -- Y.

op(setelement,A, B, C) -> setelement(A, B, C).

simple({integer,I}) -> true;
simple({float,F}) -> true;
simple({atom,A}) -> true;
simple(nil) -> true;
simple({var,V}) -> true;
simple(Other) -> false.

%% subst_var(Expr, OldVar, NewVar) -> Expr.
%%  We KNOW that all objects are tagged.

subst_var(Old, Old, New) -> New;		%Must take this first
subst_var({Tag,B}, Old, New) ->
    {Tag,subst_var(B, Old, New)};
subst_var({Tag,B0,C0}, Old, New) ->
    B = subst_var(B0, Old, New),
    C = subst_var(C0, Old, New),
    {Tag,B,C};
subst_var({Tag,B0,C0,D0}, Old, New) ->
    B = subst_var(B0, Old, New),
    C = subst_var(C0, Old, New),
    D = subst_var(D0, Old, New),
    {Tag,B,C,D};
subst_var({Tag,B0,C0,D0,E0}, Old, New) ->
    B = subst_var(B0, Old, New),
    C = subst_var(C0, Old, New),
    D = subst_var(D0, Old, New),
    E = subst_var(E0, Old, New),
    {Tag,B,C,D,E};
subst_var({Tag,B0,C0,D0,E0,F0}, Old, New) ->
    B = subst_var(B0, Old, New),
    C = subst_var(C0, Old, New),
    D = subst_var(D0, Old, New),
    E = subst_var(E0, Old, New),
    F = subst_var(F0, Old, New),
    {Tag,B,C,D,E,F};
subst_var([H|T], Old, New) ->
    [subst_var(H, Old, New)|subst_var(T, Old, New)];
subst_var([], Old, New) -> [];
subst_var(C, _, _) when constant(C) -> C.	%Catch all constants
