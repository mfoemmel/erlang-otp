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
%% Purpose : Transform normal Erlang to kernel Erlang

%% At this stage all preprocessing has been done. All that is left are
%% "pure" Erlang functions.
%%
%% Kernel transformation is done in four stages:
%%
%% 1. Flatten expressions into kernel expressions without doing
%%    matching.
%% 2. Annotate "pre-kernel" expressions with variables they
%%    use and create.
%% 3. Use annotations to generate exports correctly (must be two
%%    pass).
%% 4. Compile pattern matching into trees to get true kernel
%%    expressions.

-module(v2_kernel).

-export([module/2]).

-import(lists, [map/2,foldl/3,foldr/3,mapfoldl/3,prefix/2,
		reverse/1,reverse/2,member/2]).
-import(ordsets, [add_element/2,del_element/2,is_element/2,
		  union/1,union/2,intersection/2,subtract/2]).

-record(kern, {vcount=0,			%Variable counter
	       ret=none,			%Return type
	       fcount=0,			%Fun counter
	       funs=[],				%Fun functions
	       func}).				%Current function.

-record(u, {ke,us=[],ns=[]}).			%Variable usage annotation

module({Mod,Exp,Forms}, Options) ->
    {Attr,Kfs} = functions(Forms, [], []),
    {ok,{Mod,Exp,Attr,Kfs}}.

functions([{function,L,Name,Arity,Cs0}|T], Attr, Fs) ->
    St0 = #kern{vcount=0,funs=[],func={Name,Arity}},
    {Cs1,St1} = flatten_fun(Cs0, St0),
    %%{function,Name,Arity,Cs1};
    {Cs2,St2} = vusage_fun(Cs1, St1),
    %%{function,Name,Arity,Cs2};
    {Cs3,St3} = export_fun(Cs2, St2),
    functions(reverse(St3#kern.funs, T), Attr, [{function,Name,Arity,Cs3}|Fs]);
functions([{asm,Name,Arity,Code}=Asm|T], Attr, Fs) ->
    functions(T, Attr, [Asm|Fs]);
functions([{attribute,0,Name,Val}|T], Attr, Fs) ->
    functions(T, [{Name,Val}|Attr], Fs);
functions([], Attr, Fs) ->
    {reverse(Attr),reverse(Fs)}.


flatten_fun(Cs0, St0) ->
    flatmapfoldl(fun (C, St) -> fclause(C, St) end, St0, Cs0).

%% fclause(Clause, State) -> {Clause,State}.
%%  Flatten expressions and remove line numbers

fclause({clause,L,H0,[],B}, St) ->
    H = fhead(H0),
    fclause(H, [[]], B, St);
fclause({clause,L,H0,Gs,B}, St) ->
    H = fhead(H0),
    fclause(H, Gs, B, St).

fclause(H, [G0|Gs], B0, St0) ->
    {G1,St1} = fguard(G0, St0),
    {B1,St2} = fexprs(B0, St1),
    {Cs,St3} = fclause(H, Gs, B0, St2),
    {[{clause,H,G1,B1}|Cs],St3};
fclause(H, [], B, St) -> {[],St}.

%% fhead([P]) -> [P].

fhead(Ps) -> fpattern_list(Ps).

%% fguard([Expr], State) -> {[Kexpr],State}.
%%  Flatten guard expressions and convert top level to tests which
%%  never return values.

fguard([G0|Gs0], St0) ->
    {G1,Pgs,St1} = fguard_test(G0, St0),
    {Gs1,St2} = fguard(Gs0, St1),
    {Pgs ++ [G1|Gs1],St2};
fguard([], St) -> {[],St}.

%% fguard_test(Test, State) -> {Ktest,[PreExpr],State}.
%%  Flatten guard tests, the top-level call becomes a {test,Test,[Arg]}.

fguard_test({op,L,Op,L0,R0}, St0) ->
    {As,Aps,St1} = fcall_args([L0,R0], call, St0),
    {{test,Op,As},Aps,St1};
fguard_test({call,L,{remote,Lr,{atom,Lm,erlang},{atom,Lf,Test}},As0}, St0) ->
    {As1,Aps,St1} = fcall_args(As0, call, St0),
    {{test,Test,As1},Aps,St1}.

%% fexprs([Expr], State) -> {[Kexpr],State}.
%%  Flatten top-level exprs.

fexprs([E], St0) ->
    {E1,Pes,St1} = fexpr(E, top, St0),
    {Pes ++ [E1],St1};
fexprs([E0|Es0], St0) ->
    {E1,Pes,St1} = fexpr(E0, top, St0),
    {Es1,St2} = fexprs(Es0, St1),
    %% Remove simple top-level expressions.
    {Pes ++ case simple(E1) of
		true -> Es1;
		false -> [E1|Es1]
	    end, St2};
fexprs([], St) -> {[],St}.

%% fexpr(Expr, Where, State) -> {Kexpr,[PreExpr],State}.

fexpr({var,L,V}, W, St) -> {{var,V},[],St};
fexpr({integer,L,I}, W, St) -> {{integer,I},[],St};
fexpr({float,L,F}, W, St) -> {{float,F},[],St};
fexpr({atom,L,A}, W, St) -> {{atom,A},[],St};
fexpr({nil,L}, W, St) -> {nil,[],St};
fexpr({string,L,S}, W, St0) ->
    {New,St1} = new_var(St0),
    notset_expr({var,New}, {set,{var,New},{string,S}}, [], W, top, St1);
fexpr({cons,L,H,T}, W, St0) ->
    {Es1,Eps,St1} = fcall_args([H,T], cons, St0),
    {New,St2} = new_var(St1),
    notset_expr({var,New}, {set,{var,New},{cons,Es1}}, Eps, W, top, St2);
fexpr({tuple,L,Es0}, W, St0) ->
    {Es1,Eps,St1} = fcall_args(Es0, tuple, St0),
    {New,St2} = new_var(St1),
    notset_expr({var,New}, {set,{var,New},{tuple,Es1}}, Eps, W, top, St2);
fexpr({block,L,Es0}, W, St0) ->
    {Es1,St1} = fexprs(first(Es0), St0),
    {E1,Eps,St2} = fexpr(last(Es0), W, St1),
    {E1,Es1 ++ Eps,St2};
%% Do we want to inline blocks now?
%fexpr({block,L,Es0}, W, St0) ->
%    {Es1,St1} = fexprs(Es0, St0),
%    {New,St2} = new_var(St1),
%    notset_expr({var,New}, {block,Es1,{var,New}}, [], W, top, St2);
fexpr({'if',L,Cs0}, W, St0) ->
    {Cs1,St1} = ficr_clauses(Cs0, St0),
    {New,St2} = new_var(St1),
    notset_expr({var,New}, {'if',Cs1,{var,New}}, [], W, top, St2);
fexpr({'case',L,E0,Cs0}, W, St0) ->
    {Evar,Eps,St1} = fexpr_var(E0, call, St0),	%Needed for later matching
    {Cs1,St2} = ficr_clauses(Cs0, St1),
    {New,St3} = new_var(St2),
    notset_expr({var,New}, {'case',Evar,Cs1,{var,New}}, Eps, W, top, St3);
fexpr({'receive',L,Cs0}, W, St0) ->
    {Cs1,St1} = ficr_clauses(Cs0, St0),
    {New,St2} = new_var(St1),
    notset_expr({var,New}, {receive_loop,Cs1,{var,New}},
		[],
		W, top, St2);
fexpr({'receive',L,Cs0,Te0,Tes0}, W, St0) ->
    {Te1,Teps,St1} = fexpr(Te0, call, St0),
    {Tes1,St2} = fexprs(Tes0, St1),
    {Cs1,St3} = ficr_clauses(Cs0, St2),
    {New,St4} = new_var(St3),
    notset_expr({var,New}, {receive_loop,Te1,Cs1,Tes1,{var,New}},
		Teps,
		W, top, St4);
fexpr({'catch',L,E0}, W, St0) ->
    {E,Eps,St1} = fexpr(E0, top, St0),
    {New,St2} = new_var(St1),
    notset_expr({var,New}, {'catch',Eps ++ [E],{var,New}}, [], W, top, St2);
%    {Evar,Eps,St1} = fexpr_var(E0, call, St0),
%    {{'catch',Eps,Evar},[],St1};
fexpr({call,L,{remote,Lr,{atom,Lm,M},{atom,Lf,F}},As0}, W, St0) ->
    {As1,Aps,St1} = fcall_args(As0, call, St0),
    {New,St2} = new_var(St1),
    Call = fexpr_remote(M, F, As1, [{var,New}]),
    notset_expr({var,New}, Call, Aps, W, top, St2);
fexpr({call,L,{atom,Lf,F},As0}, W, St0) ->
    {As1,Aps,St1} = fcall_args(As0, call, St0),
    {New,St2} = new_var(St1),
    notset_expr({var,New}, {call,F,As1,[{var,New}]}, Aps, W, top, St2);
fexpr({call,L,FunExp,As0}, W, St0) ->
    {Fun,Fps,St1} = fexpr_var(FunExp, call, St0),
    {As1,Aps,St2} = fcall_args(As0, call, St1),
    {New,St3} = new_var(St2),
    Call = {call,Fun,As1,[{var,New}]},
    notset_expr({var,New}, Call, Fps ++ Aps, W, top, St3);
fexpr({'fun',Lf,{clauses,Cs},{Uniq,Hvss,Free0}}, W, St0) ->
    I = St0#kern.fcount,
    {FunName,St1} = new_fun_name(St0),
    [{clause,_,H,G,B}|_] = Cs,
    Arity = length(H) + length(Free0),
    St2 = fun_clauses(Cs, FunName, Arity, Hvss, Free0, St1),
    Free1 = make_vars(Free0, Lf),
    {Free,Aps,St3} = fcall_args(Free1, call, St2),
    {New,St4} = new_var(St3),
    Info = {FunName,Arity,I,Uniq},
    Call = {call,Info,Free,[{var,New}]},
    notset_expr({var,New}, Call, Aps, W, top, St4);
fexpr({match,L,P0,E0}, W, St0) ->
    {Evar,Eps,St1} = fexpr_var(E0, call, St0),	%Needed for later matching
    P1 = fpattern(P0),
    notset_expr(Evar, {match,P1,Evar}, Eps, W, top, St1);
fexpr({op,L,Op,A0}, W, St0) ->
    {A1,Aps,St1} = fexpr(A0, call, St0),
    {New,St2} = new_var(St1),
    notset_expr({var,New}, fexpr_op(Op, [A1], [{var,New}]), Aps, W, top, St2);
fexpr({op,L,Op,L0,R0}, W, St0) ->
    {As,Aps,St1} = fcall_args([L0,R0], call, St0),
    {New,St2} = new_var(St1),
    notset_expr({var,New},fexpr_op(Op, As, [{var,New}]), Aps, W, top, St2).

%% make_vars([VariableName], Line) -> [VariableTerm].

make_vars(Vs, Line) -> [ {var,Line,V} || V <- Vs ].

fun_clauses(Cs, Name, Arity, Hvss, Free, St0) ->
    Func = {function,0,Name,Arity,fun_clauses(Cs, Hvss, Free)},
    St0#kern{funs=[Func|St0#kern.funs]}.

fun_clauses([{clause,L,H,G,B}|Cs], [Hvs|Hvss], Free) ->
    [{clause,L,H++free_vars(Free, Hvs, L),G,B}|fun_clauses(Cs, Hvss, Free)];
fun_clauses([], [], Free) -> [].

free_vars(Vs, Hvs, Line) ->
    [ case member(V, Hvs) of
	  true -> {var,Line,'_'};
	  false -> {var,Line,V}
      end || V <- Vs ].
    
%% fexpr_remote(Module, Function, [Arg], [Ret]) -> Kexpr.
%%  Check remote calls for BIFs and get right BIF.  BIFs reside in
%%  core modules!
 
fexpr_remote(erlang, F, As, Rs) ->
    fexpr_bif(F, As, Rs);
fexpr_remote(M, F, As, Rs) ->
    {call,{remote,M,F},As,Rs}.

%% fexpr_bif(Function, [Arg], [Ret]) -> Kexpr.
%%  Get the right place for the bif.

fexpr_bif(F, As, Rs) ->
    case erl_internal:guard_bif(F, length(As)) of
	true -> {bif,F,As,Rs};
	false -> {call,{remote,erlang,F},As,Rs}
    end.

fexpr_op(Op, As, Rs) ->
    case call_op(Op, length(As)) of
	{yes, {M,F}} -> {call,{remote,M,F},As,Rs};
	no -> {bif,Op,As,Rs}
    end.

call_op('!', 2)    -> {yes, {erlang,'!'}};
call_op('++', 2)   -> {yes, {erlang,append}};
call_op('--', 2)   -> {yes, {erlang,subtract}};
call_op(Other, Ar) -> no.
	    

%% fcall_args([ArgExpr], Where, St) -> {[Arg],[PreExpr],St}.
%%  Flatten the arguments of a "call". Do this straight left to right!

fcall_args([E0|Es0], W, St0) ->
    {E1,Eps,St1} = fexpr(E0, W, St0),
    {Es1,Esps,St2} = fcall_args(Es0, W, St1),
    {[E1|Es1],Eps ++ Esps,St2};
fcall_args([], W, St) -> {[],[],St}.

ficr_clauses(Cs, St0) ->
    flatmapfoldl(fun (C, St) -> fclause(C, St) end, St0, Cs).

%% notset_expr(RetVar, Expr, PreExprs, Where, NotWhere, State) ->
%%	 {Expr,Pre,State}.
%%  Generate a 'set' expression when in SetWhare or not in NotWhere.

notset_expr(R, E, Ps, W, Not, St) when W /= Not-> {R,Ps ++ [E],St};
notset_expr(R, E, Ps, W, Not, St) -> {E,Ps,St}.

%% fexpr_var(Expr, Where, State) -> {Var,[PreExpr],State}.
%%  Flatten an expr into variable with pre-expressions.

fexpr_var(E0, W, St0) ->
    {E1,Eps,St1} = fexpr(E0, call, St0),	%Fix all but constants.
    case E1 of
	{var,R} -> {E1,Eps,St1};
	Other ->
	    {New,St2} = new_var(St1),
	    {{var,New},Eps ++ [{set,{var,New},E1}],St2}
    end.

%% fpattern(P) -> Kpat.
%% Transform a pattern by removing line numbers.  We also normalise
%% aliases in patterns to standard form, {alias,Pat,[Var]}.

fpattern({var,L,V}) -> {var,V};
fpattern({integer,L,I}) -> {integer,I};
fpattern({float,L,F}) -> {float,F};
fpattern({atom,L,A}) -> {atom,A};
fpattern({string,L,S}) -> make_string(S);
fpattern({nil,L}) -> nil;
fpattern({cons,L,H,T}) ->
    {cons,[fpattern(H),fpattern(T)]};
fpattern({tuple,L,Ps}) ->
    {tuple,fpattern_list(Ps)};
fpattern({match,L,P1,P2}) ->
    fpat_alias(fpattern(P1), fpattern(P2)).

%% fpat_alias(Pat, Pat) -> AliasPat.

fpat_alias({alias,P1,Vs1}, {alias,P2,Vs2}) ->
    {alias,fpat_alias(P1, P2),union(Vs1, Vs2)};

fpat_alias({alias,P1,Vs1}, {var,V}) ->
    {alias,P1,add_element({var,V}, Vs1)};
fpat_alias({alias,P1,Vs1}, P2) ->
    {alias,fpat_alias(P1, P2),Vs1};

fpat_alias({var,V}, {alias,P2,Vs2}) ->
    {alias,P2,add_element({var,V}, Vs2)};
fpat_alias(P1, {alias,P2,Vs2}) ->
    {alias,fpat_alias(P1, P2),Vs2};

fpat_alias(P, P) -> P;
fpat_alias({var,V}, P2) ->
    {alias,P2,[{var,V}]};
fpat_alias(P1, {var,V}) ->
    {alias,P1,[{var,V}]};

fpat_alias({cons,Ps1}, {cons,Ps2}) ->
    {cons,fpat_alias_list(Ps1, Ps2)};
fpat_alias({tuple,Ps1}, {tuple,Ps2}) ->
    {tuple,fpat_alias_list(Ps1, Ps2)}.

%fpat_alias(Pat, Pat) -> Pat;			%Trivial case
%fpat_alias(P, {var,V}) ->
%    fpat_pack_alias(P, {var,V});
%fpat_alias({var,V}, P) ->
%    fpat_pack_alias(P, {var,V});
%fpat_alias({cons,Ps1}, {cons,Ps2}) ->
%    {cons,fpat_alias_list(Ps1, Ps2)};
%fpat_alias({tuple,Ps1},{tuple,Ps2}) ->
%    {tuple,fpat_alias_list(Ps1, Ps2)};
%fpat_alias({alias,Pat1,V}, Pat2) ->
%    fpat_pack_alias(fpat_alias(Pat1, Pat2), V);
%fpat_alias(Pat1, {alias,Pat2,V}) ->
%    fpat_pack_alias(fpat_alias(Pat1, Pat2), V).

fpat_pack_alias({alias,P1,Vs}, P1) -> {alias,P1,Vs};
fpat_pack_alias({alias,P1,Vs}, V) -> {alias,P1,add_element(V, Vs)};
fpat_pack_alias(P1, {alias,P1,Vs}) -> {alias,P1,Vs};
fpat_pack_alias(V, {alias,P2,Vs}) -> {alias,P2,add_element(V, Vs)};
fpat_pack_alias(P, V) -> {alias,P,[V]}.    

%% fpat_alias_list([A1], [A2]) -> [A].

fpat_alias_list([A1|A1s], [A2|A2s]) ->
    [fpat_alias(A1, A2)|fpat_alias_list(A1s, A2s)];
fpat_alias_list([], []) -> [].

%% fpattern_list([P]) -> [P].

fpattern_list(Ps) -> map(fun fpattern/1, Ps).

%% make_string([Char]) -> StringTerm.

make_string(Cs) -> foldr(fun (C, T) -> {cons,[{integer,C},T]} end, nil, Cs).

%% first([A]) -> [A].
%% last([A]) -> A.

last([L]) -> L;
last([H|T]) -> last(T).

first([L]) -> [];
first([H|T]) -> [H|first(T)].

%% Determine the used and new variables for each Kepxr.  Unfortunately
%% we cannot directly determine the direction of variables in a
%% pattern but must keep track of which variables already exist.  We
%% also at this time make implicit variable matching explicit.

vusage_fun(Cs, St) -> uclauses(Cs, [], St).

%% uclauses([Clause], [KnownVar], State) -> {[Uclause],State}.

uclauses(Cs, Ks, St0) ->
    mapfoldl(fun (C, St) -> uclause(C, Ks, St) end, St0, Cs).

%% uclause(Clause, [KnownVar], State) -> {Uclause,State}.

uclause({clause,H0,G0,B0}, Ks0, St0) ->
    {H1,Hg,Hv,St1} = uhead(H0, Ks0, St0),
    Hu = intersection(Hv, Ks0),
    Hn = subtract(Hv, Ks0),
    Ks1 = union(Hn, Ks0),
    {Ugs,St2} = uexprs(Hg ++ G0, Ks1, St1),
    Gu = used_in_any(Ugs),
    Gn = new_in_any(Ugs),
    Ks2 = union(Gn, Ks1),
    {Ubs,St3} = uexprs(B0, Ks2, St2),
    Used = intersection(union([Hu,Gu,used_in_any(Ubs)]), Ks0),
    New = union([Hn,Gn,new_in_any(Ubs)]),
    {#u{ke={clause,H1,Ugs,Ubs},us=Used,ns=New},St3}.

%% uhead([P], [KnownVar], State) -> {[P],[Test],[Var],State}.

uhead(Ps, Ks, St) -> upattern_list(Ps, Ks, St).

%% uexprs([Kexpr], [BefVar], State) -> {[Ukexpr],State}.

uexprs(Es, Ks0, St0) ->
    {Ues,{Ks1,St1}} = mapfoldl(fun (E, {Ks,Sta}) ->
				       {Ue,Stb} = uexpr(E, Ks, Sta),
				       {Ue,{union(Ks, Ue#u.ns),Stb}}
			       end, {Ks0,St0}, Es),
    {Ues,St1}.

%% uexpr(Kexpr, [KnownVar], State) -> {Ukexpr,State}.

uexpr({set,{var,V},Con}, Ks, St) ->
    {#u{ke={set,{var,V},Con},us=con_vars(Con),ns=[V]},St};
uexpr({'case',{var,Evar},Cs,{var,Rvar}}, Ks, St0) ->
    %% Evar is already in Ks.
    {Ucs,St1} = uclauses(Cs, Ks, St0),
    Used = add_element(Evar, used_in_any(Ucs)),
    New = add_element(Rvar, new_in_all(Ucs)),
    {#u{ke={'case',{var,Evar},Ucs,{var,Rvar}},us=Used,ns=New},St1};
uexpr({'if',Cs,{var,Rvar}}, Ks, St0) ->
    {Ucs,St1} = uclauses(Cs, Ks, St0),
    Used = used_in_any(Ucs),
    New = add_element(Rvar, new_in_all(Ucs)),
    {#u{ke={'if',Ucs,{var,Rvar}},us=Used,ns=New},St1};
uexpr({receive_loop,Cs,{var,Rvar}}, Ks, St0) ->
    {Ucs,St1} = uclauses(Cs, Ks, St0),
    Used = used_in_any(Ucs),
    New = add_element(Rvar, new_in_all(Ucs)),
    {#u{ke={receive_loop,Ucs,{var,Rvar}},us=Used,ns=New},St1};
uexpr({receive_loop,Te,Cs,Tes,{var,Rvar}}, Ks, St0) ->
    {Ucs,St1} = uclauses(Cs, Ks, St0),
    {Utes,St2} = uexprs(Tes, Ks, St1),
    Used = union(used_in_any(Ucs), used_in_any(Utes)),
    New = add_element(Rvar, intersection(new_in_any(Utes), new_in_all(Ucs))),
    {#u{ke={receive_loop,Te,Ucs,Utes,{var,Rvar}},us=Used,ns=New},St2};
uexpr({'catch',Es,{var,R}}, Ks, St0) ->
    {Ues,St1} = uexprs(Es, Ks, St0),
    {#u{ke={'catch',Ues,{var,R}},us=used_in_any(Ues),ns=[R]},St1};
uexpr({call,{var,V},As,Rs}, Ks, St) ->
    {#u{ke={call,{var,V},As,Rs},
	us=simple_vars([{var,V}|As]),ns=return_vars(Rs)},St};
uexpr({call,F,As,Rs}, Ks, St) ->
    {#u{ke={call,F,As,Rs},us=simple_vars(As),ns=return_vars(Rs)},St};
uexpr({bif,F,As,Rs}, Ks, St) ->
    {#u{ke={bif,F,As,Rs},us=simple_vars(As),ns=return_vars(Rs)},St};
uexpr({match,P0,{var,Mvar}}, Ks, St0) ->
    {P1,Pg,Pv,St1} = upattern(P0, Ks, St0),
    Pu = intersection(Pv, Ks),
    Pn = subtract(Pv, Pu),
    Ke = case {P1,Pg} of
	     {{var,V},[]} -> {set,{var,V},{var,Mvar}};
	     Other -> {match,P1,Pg,{var,Mvar}}
	 end,
    {#u{ke=Ke,us=add_element(Mvar, Pu),ns=Pn},St1};
uexpr({test,Test,As}, Ks, St) ->
    {#u{ke={test,Test,As},us=simple_vars(As),ns=[]},St};
uexpr({var,V}, Ks, St) ->
    {#u{ke={var,V},us=[V],ns=[]},St};
uexpr(E, Ks, St) ->				%Catches all simple expressions
    {#u{ke=E,us=[],ns=[]},St}.

%% return_vars([var()]) -> [Var].
%% simple_vars([simple()]) -> [Var].
%% con_vars([constr()|simple()]) -> [Var].

return_vars(Rs) ->
    foldl(fun ({var,V}, Vs) -> add_element(V, Vs) end, [], Rs).

simple_vars(As) ->
    foldl(fun ({var,V}, Vs) -> add_element(V, Vs);
	      (Simple, Vs) -> Vs end, [], As).

con_vars({cons,Es}) -> simple_vars(Es);
con_vars({tuple,Es}) -> simple_vars(Es);
con_vars(Simple) -> simple_vars([Simple]).

simple({integer,I}) -> true;
simple({float,F}) -> true;
simple({atom,A}) -> true;
simple(nil) -> true;
simple({var,V}) -> true;
simple(Other) -> false.

%% used_in_any([Akexpr]) -> [Var].
%% new_in_any([Akexpr]) -> [Var].
%% new_in_all([Akexpr]) -> [Var].

used_in_any(Aes) ->
    foldl(fun (Ae, Us) -> union(Ae#u.us, Us) end, [], Aes).

new_in_any(Aes) ->
    foldl(fun (Ae, Ns) -> union(Ae#u.ns, Ns) end, [], Aes).

new_in_all([Ae|Aes]) ->
    foldl(fun (A, Ns) -> intersection(A#u.ns, Ns) end, Ae#u.ns, Aes);
new_in_all([]) -> [].

%% upattern(P, [Var], St) -> {P,[Test],[Var],St}.
%%  Transform a pattern by changing occurrences of already seen
%%  variables to new variables and an explicit test in the guard.  We
%%  do this here as we are already keeping track of variables.

upattern({var,'_'}, Vs, St0) ->
    {New,St1} = new_var(St0),
    {{var,New},[],[New],St1};
upattern({var,V}, Vs, St0) ->
    case is_element(V, Vs) of
	true ->
	    {New,St1} = new_var(St0),
	    {Tr,St2} = new_var(St1),
	    {{var,New},[{test,'=:=',[{var,V},{var,New}]}],[New],St2};
	false -> {{var,V},[],[V],St0}
    end;
upattern({cons,Ps0}, Vs, St0) ->
    {Ps1,Psg,Psv,St1} = upattern_list(Ps0, Vs, St0),
    {{cons,Ps1},Psg,Psv,St1};
upattern({tuple,Ps0}, Vs, St0) ->
    {Ps1,Psg,Psv,St1} = upattern_list(Ps0, Vs, St0),
    {{tuple,Ps1},Psg,Psv,St1};
upattern({alias,P0,V0}, Vs, St0) ->
    {P1,Pg,Pv,St1} = upattern(P0, Vs, St0),
    {V1,Vg,Vv,St2} = upattern(V0, union(Pv, Vs), St1),
    {{alias,P1,V1},Pg ++ Vg,union(Pv, Vv),St2};
upattern(Simple, Vs, St) -> {Simple,[],[],St}.

%% upattern_list([P], [Var], St) -> {[P],[Test],[NewVar],St}.

upattern_list([P0|Ps0], Vs, St0) ->
    {P1,Pg,Pv,St1} = upattern(P0, Vs, St0),
    {Ps1,Psg,Psv,St2} = upattern_list(Ps0, union(Pv, Vs), St1),
    {[P1|Ps1],Pg ++ Psg,union(Pv, Psv),St2};
upattern_list([], Vs, St) -> {[],[],[],St}.

export_fun(Acs, St0) ->
    {Cs,St1} = eclauses(Acs, [], St0#kern{ret=return}),
    export_clauses(Cs, [], St1#kern{ret=return}).

%% eclauses([Clause], [AftVar], St) -> {[Clause],St}.

eclauses([C0|Cs0], As, St0) ->
    {C1,St1} = eclause(C0, As, St0),
    {Cs1,St2} = eclauses(Cs0, As, St1),
    {[C1|Cs1],St2};
eclauses([], As, St) -> {[],St}.

%% eclause(Clause, [AftVar], St) -> {Clause,St}.

eclause(Uc, As, St0) ->
    {clause,H,Ag,Ab} = Uc#u.ke,
    {B,Bus,St1} = eexprs(Ab, As, St0),
    {G,Gus,St2} = eexprs(Ag, union(Bus, As), St1#kern{ret=break}),
    {{clause,H,G,B},St2#kern{ret=St0#kern.ret}}.

%% eexprs([annKexpr], [AftVar], St) -> {[Kexpr],[UsedVar],St}.
%%  Traverse the expression list backwards as we need to keep track of
%%  which variables are used later.  Make sure that we restore return
%%  status all but last are of state break.

eexprs(Aes, As, St0) ->
    {Kes,Us,St1} =
	foldr(fun (Ae, {Es,As0,St0}) ->
		      {E,St1} = eexpr(Ae, As0, St0),
		      {[E|Es],union(Ae#u.us, As0),St1#kern{ret=break}}
	      end, {[],As,St0}, Aes),
    {Kes,Us,St1#kern{ret=St0#kern.ret}}.

%% eexpr(annKexpr, [AftVar], St) -> {Kexpr,St}.

eexpr(Ae, As, St) -> eexpr(Ae#u.ke, Ae#u.us, Ae#u.ns, As, St).

eexpr({var,V}, Used, New, As, St) -> {{var,V},St};
eexpr({set,{var,V},Con}, Used, New, As, St) ->
    {{set,{var,V},Con},St};
eexpr({'case',Evar,Cs0,{var,Rvar}}, Used, New, As, St0) ->
    {Cs1,St1} = eclauses(Cs0, As, St0),
    Exp = del_element(Rvar, intersection(New, As)),
    {Cs2,St2} = export_clauses(Cs1, Exp, St1),
    {{'case',Evar,Cs2,[{var,Rvar}|make_vars(Exp)]},St2};
eexpr({'if',Cs0,{var,Rvar}}, Used, New, As, St0) ->
    {Cs1,St1} = eclauses(Cs0, As, St0),
    Exp = del_element(Rvar, intersection(New, As)),
    {Cs2,St2} = export_clauses(Cs1, Exp, St1),
    {{'if',Cs2,[{var,Rvar}|make_vars(Exp)]},St2};
eexpr({receive_loop,Acs,{var,Rvar}}, Used, New, As, St0) ->
    {Cs0,St1} = eclauses(Acs, As, St0),
    Exp = del_element(Rvar, intersection(New, As)),
    {Cs1,St2} = export_clauses(Cs0, Exp, St1),
    {{receive_loop,{atom,infinity},Cs1,[],[{var,Rvar}|make_vars(Exp)]},St2};
eexpr({receive_loop,Te,Acs,Ates,{var,Rvar}}, Used, New, As, St0) ->
    {Cs0,St1} = eclauses(Acs, As, St0),
    {Tes0,Tesu,St2} = eexprs(Ates, As, St1),
    Exp = del_element(Rvar, intersection(New, As)),
    {Cs1,St3} = export_clauses(Cs0, Exp, St2),
    {Tes1,St4} = export_body(Tes0, Exp, St3),
    {{receive_loop,Te,Cs1,Tes1,[{var,Rvar}|make_vars(Exp)]},St4};
eexpr({'catch',Aes,{var,R}}, Used, New, As, St0) ->
    %% NEVER return out of a catch!
    {Es0,Esu,St1} = eexprs(Aes, As, St0#kern{ret=break}),
    {Es1,St2} = export_body(Es0, [], St1),
    {{'catch',Es1,{var,R}},St1#kern{ret=St0#kern.ret}};
eexpr({call,F,As,Rs}, Used, New, Vs, St) ->
    {{call,F,As,Rs},St};
eexpr({bif,F,As,Rs}, Used, New, Vs, St) ->
    {{bif,F,As,Rs},St};
eexpr({match,P,G,{var,Mvar}}, Used, New, Vs, St) ->
    {{match,P,G,{var,Mvar}},St};
eexpr(E, Used, New, Vs, St) -> {E,St}.		%Catches all simple expressions

%% export_clauses([Clause], [Var], State) -> {[Clause],State}.
%%  Add a break after the last kexpr in the body of each clause,
%%  include exported variables AFTER return value. N.B. export
%%  variables are derived for whole scope so no problems with
%%  guaranteeing same order.

export_clauses(Cs, Exp, St) ->
    mapfoldl(fun (C, St0) -> export_clause(C, Exp, St0) end, St, Cs).

export_clause({clause,H,G,B0}, Exp, St0) ->
    {B1,St1} = export_body(B0, Exp, St0),
    {{clause,H,G,B1},St1}.

export_body([E], Exp, St) ->
    case St#kern.ret of
	return -> export_last_return(E, make_vars(Exp), St);
	break -> export_last_break(E, make_vars(Exp), St)
    end;
export_body([E|Es0], Exp, St0) ->
    {Es1,St1} = export_body(Es0, Exp, St0),
    {[E|Es1],St1}.

export_last_return({'case',E,Cs,Rs}, Exp, St) ->
    Les = case prefix(Exp, tl(Rs)) of
	      true -> [{'case',E,Cs,[]}];
	      false -> [{'case',E,Cs,Rs},{return,[hd(Rs)|Exp]}]
	  end,
    {Les,St};
export_last_return({'if',Cs,Rs}, Exp, St) ->
    Les = case prefix(Exp, tl(Rs)) of
	      true -> [{'if',Cs,[]}];
	      false -> [{'if',Cs,Rs},{return,[hd(Rs)|Exp]}]
	  end,
    {Les,St};
export_last_return({'catch',Es,R}, Exp, St) ->
    {[{'catch',Es,R},{return,[R|Exp]}],St};
export_last_return({receive_loop,Te,Cs,Tes,Rs}, Exp, St) ->
    Les = case prefix(Exp, tl(Rs)) of
	      true -> [{receive_loop,Te,Cs,Tes,[]}];
	      false -> [{receive_loop,Te,Cs,Tes,Rs},{return,[hd(Rs)|Exp]}]
	  end,
    {Les,St};
export_last_return({call,F,As,Rs}, Exp, St) ->
    Les = case prefix(Exp, tl(Rs)) of
	      true -> [{enter,F,As}];
	      false -> [{call,F,As,Rs},{return,[hd(Rs)|Exp]}]
	  end,
    {Les,St};
export_last_return({bif,B,As,Rs}, Exp, St) ->
    {[{bif,B,As,Rs},{return,[hd(Rs)|Exp]}],St};
export_last_return({set,V,E}, Exp, St) ->
    {[{set,V,E},{return,[V|Exp]}],St};
export_last_return(E, Exp, St) ->
    case simple(E) of
	true -> {[{return,[E|Exp]}],St};
	false -> {[E,{return,[element(size(E), E)|Exp]}],St}
    end.

export_last_break({'case',E,Cs,Rs}, Exp, St) ->
    {[{'case',E,Cs,Rs},{break,[hd(Rs)|Exp]}],St};
export_last_break({'if',Cs,Rs}, Exp, St) ->
    {[{'if',Cs,Rs},{break,[hd(Rs)|Exp]}],St};
export_last_break({'catch',Es,R}, Exp, St) ->
    {[{'catch',Es,R},{break,[R|Exp]}],St};
export_last_break({receive_loop,Te,Cs,Tes,Rs}, Exp, St) ->
    {[{receive_loop,Te,Cs,Tes,Rs},{break,[hd(Rs)|Exp]}],St};
export_last_break({call,F,As,Rs}, Exp, St) ->
    Les = if
	      St#kern.ret == return -> [{enter,F,As,Rs}];
	      true -> [{call,F,As,Rs},{break,[hd(Rs)|Exp]}]
	  end,
    {Les,St};
export_last_break({bif,B,As,Rs}, Exp, St) ->
    {[{bif,B,As,Rs},{break,[hd(Rs)|Exp]}],St};
export_last_break({set,V,E}, Exp, St) ->
    {[{set,V,E},{break,[V|Exp]}],St};
export_last_break(E, Exp, St) ->
    case simple(E) of
	true -> {[{break,[E|Exp]}],St};
	false -> {[E,{break,[element(size(E), E)|Exp]}],St}
    end.

%% make_vars([Name]) -> [{Var,Name}].

make_vars(Vs) -> [ {var,V} || V <- Vs ].

%% new_var(State) -> {VarName,State}.

new_var(St) ->
    C = St#kern.vcount,
    {list_to_atom("ker" ++ integer_to_list(C)),St#kern{vcount=C + 1}}.

break_type(St) -> St#kern.ret.			%Use smart names here

%% new_fun_name(State) -> {FunName,State}.

new_fun_name(#kern{func={F,A},fcount=I}=St) ->
    Name = "-" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A)
	++ "-fun-" ++ integer_to_list(I) ++ "-",
    {list_to_atom(Name),St#kern{fcount=I+1}}.

flatmapfoldl(F, Accu0, [Hd|Tail]) ->
    {R,Accu1} = F(Hd, Accu0),
    {Rs,Accu2} = flatmapfoldl(F, Accu1, Tail),
    {R++Rs,Accu2};
flatmapfoldl(F, Accu, []) -> {[],Accu}.
