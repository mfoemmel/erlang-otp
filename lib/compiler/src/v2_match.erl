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
%% Purpose : Compile pattern matching into trees.

%% This code implements the algorithm for an optimizing compiler for
%% pattern matching given "The Implementation of Functional
%% Programming Languages" by Simon Peyton Jones. The code is much
%% longer as the meaning of constructors is different from the book.
%%
%% In Erlang many constructors can have different values, e.g. 'atom'
%% or 'integer', whereas in the original algorithm thse would be
%% different constructors. Our view makes it easier in later passes to
%% handle indexing over each type.
%%
%% Patterns are complicated by having alias variables.  The form of a
%% pattern is Pat | {alias,Pat,[AliasVar]}.  This is hidden by access
%% functions to pattern arguments but the code must be aware of it.

-module(v2_match).

-export([module/2]).

-import(lists, [map/2,foldl/3,foldr/3,mapfoldl/3,splitwith/2]).

-record(match, {vcount=0}).			%Variable counter

module({Mod,Exp,Attr,Forms}, Options) ->
    Mfs = map(fun function/1, Forms),
    {ok, {Mod,Exp,Attr,Mfs}}.

function({function,Name,Arity,Cs0}) ->
    St0 = #match{vcount=0},
    {Cs1,St1} = mapfoldl(fun (C, St) -> mclause(C, St) end, St0, Cs0),
    {Mvs,St2} = make_vars(Arity, St1),
    {Mexpr,St3} = match(Mvs, Cs1, {match_fail,function_clause}, St2),
    {function,Name,Mvs,[{scope,Mexpr,[]}]};
function({asm,Name,Arity,Code}=Asm) ->
    Asm.

%% -type mclause(Clause, St) -> {Clause,St}.

mclause({clause,H,G,B0}, St0) ->
    {B1,St1} = mexprs(B0, St0),
    {{clause,H,G,B1},St1}.    

%% -type mexprs([kexpr()], St) -> {[kexpr()],St}.

mexprs([{match,{var,V},[],Mvar}|Kes0], St0) ->
    {Kes1,St1} = mexprs(Kes0, St0),
    {[{set,{var,V},Mvar}|Kes1],St1};
mexprs([{match,P,G,Mvar}|Kes0], St0) ->
    {Kes1,St1} = mexprs(Kes0, St0),
    {Mexpr,St2} = match([Mvar], [{clause,[P],G,Kes1}],
			{match_fail,{badmatch,Mvar}}, St1),
    {R,St3} = new_var(St2),
    {[{scope,Mexpr,[{var,R}]},{break,[{var,R}]}],St3};
mexprs([Ke0|Kes0], St0) ->
    {Ke1,St1} = mexpr(Ke0, St0),
    {Kes1,St2} = mexprs(Kes0, St1),
    {[Ke1|Kes1],St2};
mexprs([], St) -> {[],St}.

%% -type mexpr(kexpr(), St) -> {kexpr(),St}.

mexpr({block,Es0,Rs}, St0) ->
    {Es1,St1} = mexprs(Es0, St0),
    {{scope,{block,Es1},Rs},St1};
mexpr({'if',Cs0,Rs}, St0) ->
    {Cs1,St1} = micr_clauses(Cs0, St0),
    {Iexpr,St2} = match_if(Cs1, {match_fail,if_clause}, St1),
    {{scope,Iexpr,Rs},St2};
mexpr({'case',Cvar,Cs0,Rs}, St0) ->
    {Cs1,St1} = micr_clauses(Cs0, St0),
    {Cexpr,St2} = match([Cvar], Cs1,
			{match_fail,{case_clause,Cvar}}, St1),
    {{scope,Cexpr,Rs},St2};
mexpr({receive_loop,{atom,infinity}=To,Cs,Tes,Rs}, St) when Tes =/= [] ->
    mexpr({receive_loop,To,Cs,[],Rs}, St);
mexpr({receive_loop,To,Cs0,Tes0,Rs}, St0) ->
    {Cs1,St1} = micr_clauses(Cs0, St0),
    {Tes1,St2} = mexprs(Tes0, St1),
    {Rvar,St3} = new_var(St2),
    {Rm,St4} = match([{var,Rvar}], prepend_body([receive_accept], Cs1),
		     {block,[receive_reject,receive_next]}, St3),
    {{receive_loop,To,{var,Rvar},Rm,Tes1,Rs},St4};
mexpr({'catch',Es0,{var,R}}, St0) ->
    {Es1,St1} = mexprs(Es0, St0),
    {{'catch',Es1,{var,R}},St1};
mexpr({call,F,As,Rs}, St) -> {{call,F,As,Rs},St};
mexpr({bif,F,As,Rs}, St) -> {{bif,F,As,Rs},St};
mexpr(Ke, St) -> {Ke,St}.

micr_clauses(Cs, St) -> mapfoldl(fun mclause/2, St, Cs).

%% -type prepend_body([Expr], [Clause]) -> [Clause].

prepend_body(Es, Cs) ->
    map(fun ({clause,H,G,B}) -> {clause,H,G,Es ++ B} end, Cs).

%% new_var(State) -> {VarName,State}.
%%  Create a new varaible name.

new_var(St) ->
    C = St#match.vcount,
    {list_to_atom("mat" ++ integer_to_list(C)),St#match{vcount=C + 1}}.

%% make_vars(Count, State) -> {[Var],State}.
%%  Make Count new variables.

make_vars(N, St) -> make_vars(N, St, []).

make_vars(N, St0, Vs) when N > 0 ->
    {V,St1} = new_var(St0),
    make_vars(N-1, St1, [{var,V}|Vs]);
make_vars(0, St, Vs) -> {Vs,St}.

%% match([Var], [Clause], Default, State) -> {MatchExpr,State}.

match([U|Us], Cs, Def, St0) ->
    Pcss = partition(Cs),
    foldr(fun (Pcs, {D,St}) -> match_varcon([U|Us], Pcs, D, St) end,
	  {Def,St0}, Pcss);
match([], Cs, Def, St) ->
    match_guard(Cs, Def, St).

%% match_if([Clause], Default, State) -> {IfExpr,State}.

match_if(Cs, Def, St) -> match_guard(Cs, Def, St).

%% match_guard([Clause], Default, State) -> {IfExpr,State}.
%%  Build a guard to handle guards. A guard *ALWAYS* fails if no
%%  clause matches, there will be a surrounding 'try' to catch the
%%  failure. Catch redundant case.

match_guard(Cs0, Def0, St) ->
    {Cs1,Def1} = foldr(fun match_guard/2, {[],Def0}, Cs0),
    {build_try(build_guard(Cs1), Def1),St}.

match_guard({clause,[],[],B}, {Cs,Def}) ->
    {[],{block,B}};
match_guard({clause,[],G,B}, {Cs,Def}) ->
    {[{gclause,G,{block,B}}|Cs],Def}.

%% partition([Clause]) -> [[Clause]].
%%  Partition a list of clauses into groups which either contain
%%  clauses with a variable first argument, or with a "constructor".

partition([C1|Cs]) ->
    V1 = is_var_clause(C1),
    {More,Rest} = splitwith(fun (C) -> is_var_clause(C) == V1 end, Cs),
    [[C1|More]|partition(Rest)];
partition([]) -> [].

%% match_varcon([Var], [Clause], Def, State) -> {MatchExpr,State}.

match_varcon(Us, [C|Cs], Def, St) ->
    case is_var_clause(C) of
	true -> match_var(Us, [C|Cs], Def, St);
	false -> match_con(Us, [C|Cs], Def, St)
    end.

%% match_var([Var], [Clause], Def, State) -> {MatchExpr,State}.
%%  Build a call to "select" from a list of clauses all containing a
%%  variable as the first argument.  We must rename the variable in
%%  each clause to be the match variable as these clause will share
%%  this variable and may have differnet names for it.  Rename aliases
%%  as well.

match_var([U|Us], Cs, Def, St) ->
    match(Us,
	  map(fun ({clause,[Arg|As],G,B}) ->
		      Vs = [arg_arg(Arg)|arg_alias(Arg)],
		      {clause,As,subst_vars(G, Vs, U),subst_vars(B, Vs, U)}
	      end, Cs),
	  Def, St).

%% match_con(Variables, [Clause], Default, State) -> {SelectExpr,State}.
%%  Build call to "select" from a list of clauses all containing a
%%  constructor/constant as first argument.  Group the constructors
%%  according to type, the order is really irrelevant but tries to be
%%  smart.

match_con([U|Us], Cs, Def, St0) ->
    %% Extract clauses for different constructors (types).
    Ttcs = [ {T,Tcs} || T <- [cons,tuple,atom,float,integer,nil],
		       begin Tcs = select(T, Cs),
			     Tcs /= []
		       end ],
    {Scs,St1} =
	mapfoldl(fun ({T,Tcs}, St) ->
			 {Sc,S1} = match_value([U|Us], T, Tcs, fail, St),
			 {{T,Sc},S1} end,
		 St0, Ttcs),
    {build_try(build_select(U, Scs), Def),St1}.

%% select(Con, [Clause]) -> [Clause].

select(T, Cs) -> [ C || C <- Cs, clause_con(C) == T ].

%% match_value([Var], Con, [Clause], Default, State) -> {SelectExpr,State}.
%%  At this point all the clauses have the same constructor, we must
%%  now separate them according to value.

match_value(Us, T, [], Def, St) -> {[],St};
match_value(Us, T, Cs0, Def, St0) ->
    Css = group_value(T, Cs0),
    mapfoldl(fun (Cs, St) -> match_clause(Us, T, Cs, Def, St) end, St0, Css).

%% group_value([Clause]) -> [[Clause]].
%%  Group clauses according to value.

group_value(cons, Cs) -> [Cs];			%These are single valued
group_value(nil, Cs) -> [Cs];
group_value(T, Cs) -> group_value(Cs).

group_value([C|Cs]) ->
    V = clause_val(C),
    Same = [ Cv || Cv <- Cs, clause_val(Cv) == V ], %Same value
    Rest = [ Cv || Cv <- Cs, clause_val(Cv) /= V ], % and all the rest
    [[C|Same]|group_value(Rest)];
group_value([]) -> [].

%% match_clause([Var], Con, [Clause], Default, State) -> {Clause,State}.
%%  At this point all the clauses have the same "value".  Build one
%%  select clause for this value and continue matching.  Rename
%%  aliases as well.

match_clause([U|Us], T, Cs0, Def, St0) ->
    {Match,Vs,St1} = get_match(get_con(Cs0), St0),
    {B,St2} = match(Vs ++ Us, new_clauses(Cs0, U), Def, St1),
    {{sclause,Match,B},St2}.

get_con([C|Cs]) -> arg_arg(clause_arg(C)).	%Get the constructor

get_match({cons,Es}, St0) ->
    {Mes,St1} = make_vars(2, St0),
    {{cons,Mes},Mes,St1};
get_match({tuple,Es}, St0) ->
    {Mes,St1} = make_vars(length(Es), St0),
    {{tuple,Mes},Mes,St1};
get_match(M, St) -> {M,[],St}.

new_clauses(Cs, U) ->
    map(fun ({clause,[Arg|As],G,B}) ->
		Head = case arg_arg(Arg) of
			   {cons,Es} -> Es ++ As;
			   {tuple,Es} -> Es ++ As;
			   Other -> As
		       end,
		Alias = arg_alias(Arg),
		{clause,Head,subst_vars(G, Alias, U),subst_vars(B, Alias, U)}
	end, Cs).

%%new_clauses(Cs) -> map(fun new_clause/1, Cs).

%%new_clause({clause,[{cons,Es}|Head],G,B}) ->
%%    {clause,Es ++ Head,G,B};
%%new_clause({clause,[{tuple,Es}|Head],G,B}) ->
%%    {clause,Es ++ Head,G,B};
%%new_clause({clause,[_Other|Head],G,B}) ->
%%    {clause,Head,G,B}.

%% build_guard([GuardClause]) -> GuardExpr.

build_guard([]) -> fail;
build_guard(Cs) -> {guard,Cs}.

%% build_select(Var, [ConClause]) -> SelectExpr.

build_select({var,V}, Tcs) -> {select,{var,V},Tcs};
build_select(U, Tcs) -> select_const(U, Tcs).

select_const({Type,Val}=TypeVal, [{Type,Scs0}|_]) -> select_const_val(TypeVal, Scs0);
select_const(nil, [{nil,Scs0}|_]) -> select_const_val(nil, Scs0);
select_const(Value, [WrongType|Scs0]) -> select_const(Value, Scs0);
select_const(Value, []) -> fail.

select_const_val(TypeVal, [{sclause,TypeVal,B}|_]) -> B;
select_const_val(TypeVal, [WrongVal|Scs0]) -> select_const_val(TypeVal, Scs0);
select_const_val(TypeVal, []) -> fail.

%% build_try(First, Second) -> TryExpr.
%%  Build a try, attempt some simple optimisation.

build_try(fail, Second) -> Second;
build_try(First, fail) -> First;
build_try(First, Second) -> {try,First,Second}.

%% clause_arg(Clause) -> FirstArg.
%% clause_con(Clause) -> Constructor.
%% clause_val(Clause) -> Value.
%% is_var_clause(Clause) -> bool().

clause_arg({clause,[Arg|_],G,B}) -> Arg.

clause_con(C) -> arg_con(clause_arg(C)).

clause_val(C) -> arg_val(clause_arg(C)).

is_var_clause(C) -> clause_con(C) == var.

%% arg_arg(Arg) -> Arg.
%% arg_alias(Arg) -> Aliases.
%% arg_con(Arg) -> Constructor.
%% arg_val(Arg) -> Value.
%%  These are the basic functions for obtaining fields in an argument.

arg_arg({alias,Con,As}) -> Con;
arg_arg(Con) -> Con.

arg_alias({alias,Con,As}) -> As;
arg_alias(Con) -> [].

arg_con(Arg) ->
    case arg_arg(Arg) of
	{cons,Es} -> cons; 
	{tuple,Es} -> tuple;
	{T,V} -> T;
	nil -> nil
    end.

arg_val(Arg) ->
    case arg_arg(Arg) of
	{cons,Es} -> 2; 
	{tuple,Es} -> length(Es);
	{T,V} -> V;
	nil -> 0
    end.

%% subst_vars(Expr, [OldVar], NewVar) -> Expr.
%%  Substitute NewVar for all occurences of OldVar in Expr.

subst_vars(E0, Os, New) ->
    foldl(fun (O, E) -> subst_var(E, O, New) end, E0, Os).

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
