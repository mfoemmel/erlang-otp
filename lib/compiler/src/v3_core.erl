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
%% Purpose : Transform normal Erlang to Core Erlang

%% At this stage all preprocessing has been done. All that is left are
%% "pure" Erlang functions.
%%
%% Core transformation is done in four stages:
%%
%% 1. Flatten expressions into an internal core form without doing
%%    matching.
%%
%% 2. Step "forwards" over the icore code annotating each "top-level"
%%    thing with variable usage.  Detect bound variables in matching
%%    and replace with explicit guard test.  Annotate "internal-core"
%%    expressions with variables they use and create.  Convert matches
%%    to cases when not pure assignments.
%%
%% 3. Step "backwards" over icore code using variable usage
%%    annotations to change implicit exported variables to explicit
%%    returns.
%%
%% We have to be very careful with matches as these create variables.
%% While we try not to flatten things more than necessary we must make
%% sure that all matches are at the top level.  For this we use the
%% type "simple" which are non-match expressions.  Cases and receives
%% can also create problems due to exports variables so they are not
%% "simple" either.  I.e. a simple will not export variables.
%%
%% Annotations in the icore code is kept in a record, #a, not in a
%% list as in proper core.  This is easier and faster and creates no
%% problems as we have complete control over all annotations.
%%
%% In this translation:
%%
%% call ops are literals
%% call arguments are literals
%% match arguments are simples
%% case arguments are simples
%% receive timeouts are simples
%% let/set arguments are expressions
%% fun is not a literal

-module(v3_core).

-export([module/2]).

-import(lists, [map/2,foldl/3,foldr/3,mapfoldl/3]).
-import(ordsets, [add_element/2,del_element/2,is_element/2,
		  union/1,union/2,intersection/2,subtract/2]).

-include("core_parse.hrl").

%% Internal core expressions and help functions.

-record(iset, {anno=[],var,arg}).
-record(imatch, {anno=[],pat,guard=[],arg,fc}).
-record(icase, {anno=[],args,clauses,fc}).
-record(iclause, {anno=[],pats,guard,body}).
-record(ifun, {anno=[],id,vars,body}).
-record(icall, {anno=[],op,args}).
-record(icatch, {anno=[],body}).
-record(ireceive1, {anno=[],clauses}).
-record(ireceive2, {anno=[],clauses,timeout,action}).

get_ianno(Lthing) -> element(2, Lthing).
set_ianno(Lthing, Anno) -> setelement(2, Lthing, Anno). 
clear_ianno(Lthing) -> setelement(2, Lthing, []).

-record(core, {vcount=0}).			%Variable counter

-record(a, {us=[],ns=[],anno=[]}).		%Internal annotation

module({Mod,Exp,Forms}, Options) ->
    {Kfs,As} = foldr(fun form/2, {[],[]}, Forms),
    {ok,#c_mdef{name=Mod,exports=Exp,attributes=As,body=Kfs}}.

form({function,L,N,A,Cs}=F, {Fs,As}) ->
    {[function(F)|Fs],As};
form({attribute,L,N,V}=F, {Fs,As}) ->
    {Fs,[attribute(F)|As]}.

attribute({attribute,L,Name,Val}) ->
    {Name,Val}.

function({function,L,Name,Arity,Cs0}) ->
    %% ok = io:fwrite("~p - ", [{Name,Arity}]),
    St0 = #core{vcount=0},
    {B0,St1} = body(Cs0, Arity, St0),
    %% ok = io:fwrite("1", []),
    {B1,St2} = ubody(B0, St1),
    %% ok = io:fwrite("2", []),
    {B2,St3} = cbody(B1, St2),
    %% ok = io:fwrite("3~n", []),
    #c_fdef{func=Name,arity=Arity,body=B2}.

body(Cs0, Arity, St0) ->
    {Args,St1} = new_vars(Arity, St0),
    {Cs1,St2} = clauses(Cs0, St1),
    {Ps,St3} = new_vars(Arity, St2),		%Need new variables here
    Fc = fail_clause(Ps, #c_atom{name=function_clause}),
    {#ifun{id=[],vars=Args,body=[#icase{args=Args,clauses=Cs1,fc=Fc}]},St3}.

clauses(Cs0, St0) ->
    Cs1 = expand_clauses(Cs0),
    mapfoldl(fun (C, St) -> clause(C, St) end, St0, Cs1).

expand_clauses([{clause,L,H,[G],B}|Cs]) ->
    [{clause,L,H,G,B}|expand_clauses(Cs)];
expand_clauses([{clause,L,H,[G|Gs],B}|Cs]) ->
    [{clause,L,H,G,B}|expand_clauses([{clause,L,H,Gs,B}|Cs])];
expand_clauses([{clause,L,H,[],B}|Cs]) ->
    [{clause,L,H,[],B}|expand_clauses(Cs)];
expand_clauses([]) -> [].

clause({clause,L,H0,G0,B0}, St0) ->
    H1 = head(H0),
    {G1,St1} = guard(G0, St0),
    {B1,St2} = exprs(B0, St1),
    {#iclause{pats=H1,guard=G1,body=B1},St2}.

%% head([P]) -> [P].

head(Ps) -> pattern_list(Ps).

%% guard([Expr], State) -> {[Kexpr],State}.
%%  Flatten guard expressions and convert top level to tests which
%%  never return values.

guard([E0|Es0], St0) ->
    {E1,Eps,St1} = expr(E0, St0),
    {Es1,St2} = guard(Es0, St1),
    {Eps ++ [E1] ++ Es1,St2};
guard([], St) -> {[],St}.

%% exprs([Expr], State) -> {Cexpr,State}.
%%  Flatten top-level exprs.

exprs([E0|Es0], St0) ->
    {E1,Eps,St1} = expr(E0, St0),
    {Es1,St2} = exprs(Es0, St1),
    {Eps ++ [E1] ++ Es1,St2};
exprs([], St) -> {[],St}.

%% expr(Expr, State) -> {Cexpr,[PreExp],State}.
%%  Generate an internal core expression.

expr({var,L,V}, St) -> {#c_var{name=V},[],St};
expr({integer,L,I}, St) -> {#c_int{val=I},[],St};
expr({float,L,F}, St) -> {#c_float{val=F},[],St};
expr({atom,L,A}, St) -> {#c_atom{name=A},[],St};
expr({nil,L}, St) -> {#c_nil{},[],St};
expr({string,L,S}, St) -> {#c_string{val=S},[],St};
expr({cons,L,H0,T0}, St0) ->
    {H1,Hps,St1} = literal(H0, St0),
    {T1,Tps,St2} = literal(T0, St1),
    {#c_cons{head=H1,tail=T1},Hps ++ Tps,St2};
expr({tuple,L,Es0}, St0) ->
    {Es1,Eps,St1} = lit_list(Es0, St0),
    {#c_tuple{es=Es1},Eps,St1};
expr({block,L,Es0}, St0) ->
    %% Inline the block directly.
    {Es1,St1} = exprs(first(Es0), St0),
    {E1,Eps,St2} = expr(last(Es0), St1),
    {E1,Es1 ++ Eps,St2};
expr({'if',L,Cs0}, St0) ->
    {Cs1,St1} = clauses(Cs0, St0),
    Fc = fail_clause([], #c_atom{name=if_clause}),
    {#icase{args=[],clauses=Cs1,fc=Fc},[],St1};
expr({'case',L,E0,Cs0}, St0) ->
    {E1,Eps,St1} = simple(E0, St0),
    {Cs1,St2} = clauses(Cs0, St1),
    {Fpat,St3} = new_var(St2),
    Fc = fail_clause([Fpat], #c_tuple{es=[#c_atom{name=case_clause},Fpat]}),
    {#icase{args=[E1],clauses=Cs1,fc=Fc},Eps,St3};
expr({'receive',L,Cs0}, St0) ->
    {Cs1,St1} = clauses(Cs0, St0),
    {#ireceive1{clauses=Cs1}, [], St1};
expr({'receive',L,Cs0,Te0,Tes0}, St0) ->
    {Te1,Teps,St1} = simple(Te0, St0),
    {Tes1,St2} = exprs(Tes0, St1),
    {Cs1,St3} = clauses(Cs0, St2),
    {#ireceive2{clauses=Cs1,timeout=Te1,action=Tes1},Teps,St3};
expr({'catch',L,E0}, St0) ->
    {E1,Eps,St1} = expr(E0, St0),
    {#icatch{body=Eps ++ [E1]},[],St1};
%%fexpr({'fun',L,{function,F,A}}, W, St) ->
%%    {#c_local{name=F,arity=A},[],St};
expr({'fun',L,{clauses,Cs},{Uniq,Hvs,Free}}, St) ->
    %% Take the new hacky format.
    fun_tq(Uniq, Cs, St);
expr({'fun',L,{clauses,Cs}}, St) ->
    %% Take the old hacky format.
    fun_tq(L, Cs, St);
expr({call,L,{remote,Lr,{atom,Lm,M},{atom,Lf,F}},As0}, St0) ->
    {As1,Aps,St1} = lit_list(As0, St0),
    Call = #c_remote{mod=M,name=F,arity=length(As1)},
    {#icall{op=Call,args=As1},Aps,St1};
expr({call,L,{atom,Lf,F},As0}, St0) ->
    {As1,Aps,St1} = lit_list(As0, St0),
    Call = #c_local{name=F,arity=length(As1)},
    {#icall{op=Call,args=As1},Aps,St1};
expr({call,L,FunExp,As0}, St0) ->
    {Fun,Fps,St1} = literal(FunExp, St0),
    {As1,Aps,St2} = lit_list(As0, St1),
    {#icall{op=Fun,args=As1},Fps ++ Aps,St2};
expr({match,L,P0,E0}, St0) ->
    %% First fold matches together to create aliases.
    {P1,E1} = fold_match(E0, P0),
    {E2,Eps,St1} = simple(E1, St0),
    P2 = pattern(P1),
    {Fpat,St2} = new_var(St1),
    Fc = fail_clause([Fpat], #c_tuple{es=[#c_atom{name=badmatch},Fpat]}),
    {#imatch{pat=P2,arg=E2,fc=Fc},Eps,St2};
expr({op,L,Op,A0}, St0) ->
    {A1,Aps,St1} = literal(A0, St0),
    Call = #c_remote{mod=erlang,name=Op,arity=1},
    {#icall{op=Call,args=[A1]},Aps,St1};
expr({op,L,Op,L0,R0}, St0) ->
    {As,Aps,St1} = lit_list([L0,R0], St0),
    Call = #c_remote{mod=erlang,name=Op,arity=2},
    {#icall{op=Call,args=As},Aps,St1}.

%% fun_tq(Id, [Clauses], State) -> {Fun,[PreExp],State}.

fun_tq(Id, Cs0, St0) ->
    {Cs1,St1} = clauses(Cs0, St0),
    Arity = length((hd(Cs1))#iclause.pats),
    {Args,St2} = new_vars(Arity, St1),
    {Ps,St3} = new_vars(Arity, St2),		%Need new variables here
    Fc = fail_clause(Ps, #c_atom{name=function_clause}),
    Fun = #ifun{id=[{id,Id}],				%We KNOW!
		vars=Args,body=[#icase{args=Args,clauses=Cs1,fc=Fc}]},
    {Fun,[],St3}.

%% simple(Expr, State) -> {Simple,[PreExpr],State}.
%%  Generate a simple expression, basically a call or a literal.

simple(E0, St0) ->
    {E1,Eps,St1} = expr(E0, St0),
    {Se,Sps,St2} = force_simple(E1, St1),
    {Se,Eps ++ Sps,St2}.

force_simple(#icall{}=Call, St) -> {Call,[],St};
force_simple(#ifun{}=Fun, St) -> {Fun,[],St};	%These are simple too
force_simple(Ce, St) ->
    force_literal(Ce, St).

%% literal(Expr, State) -> {Literal,[PreExpr],State}.
%%  Generate an internal literal expression.  Must do special things
%%  with matches here.

literal(E0, St0) ->
    {E1,Eps,St1} = expr(E0, St0),
    {Le,Lps,St2} = force_literal(E1, St1),
    {Le,Eps ++ Lps,St2}.

force_literal(#imatch{pat=P,arg=E,fc=Fc}, St0) ->
    {Le,Lps,St1} = force_literal(E, St0),
    {Le,Lps ++ [#imatch{pat=P,arg=Le,fc=Fc}],St1};
%%force_literal(#ifun{}=Fun, St) ->
%%    {Fun,[],St};
force_literal(Ce, St0) ->
    case is_literal(Ce) of
	true -> {Ce,[],St0};
	false ->
	    {V,St1} = new_var(St0),
	    {V,[#iset{var=V,arg=Ce}],St1}
    end.

lit_list(Es, St) ->
    foldr(fun (E, {Ces,Esp,St0}) ->
		  {Ce,Ep,St1} = literal(E, St0),
		  {[Ce|Ces],Ep ++ Esp,St1}
	  end, {[],[],St}, Es).

is_literal(#c_var{}) -> true;
is_literal(#c_int{}) -> true;
is_literal(#c_float{}) -> true;
is_literal(#c_atom{}) -> true;
is_literal(#c_char{}) -> true;
is_literal(#c_nil{}) -> true;
is_literal(#c_string{}) -> true;
is_literal(#c_cons{}) -> true;
is_literal(#c_tuple{}) -> true;
is_literal(Other) -> false.

%% fold_match(MatchExpr, Pat) -> {MatchPat,Expr}.
%%  Fold nested matches into one match with aliased patterns.

fold_match({match,L,P0,E0}, P) ->
    {P1,E1} = fold_match(E0, P),
    {{match,L,P0,P1},E1};
fold_match(E, P) -> {P,E}.

%% pattern(P) -> Kpat.
%% Transform a pattern by removing line numbers.  We also normalise
%% aliases in patterns to standard form, {alias,Pat,[Var]}.

pattern({var,L,V}) -> #c_var{name=V};
pattern({integer,L,I}) -> #c_int{val=I};
pattern({float,L,F}) -> #c_float{val=F};
pattern({atom,L,A}) -> #c_atom{name=A};
pattern({string,L,S}) -> #c_string{val=S};
pattern({nil,L}) -> #c_nil{};
pattern({cons,L,H,T}) ->
    #c_cons{head=pattern(H),tail=pattern(T)};
pattern({tuple,L,Ps}) ->
    #c_tuple{es=pattern_list(Ps)};
pattern({match,L,P1,P2}) ->
    pat_alias(pattern(P1), pattern(P2)).

%% pat_alias(Pat, Pat) -> AliasPat.

pat_alias(#c_var{name=V1}, P2) -> #c_alias{var=#c_var{name=V1},pat=P2};
pat_alias(P1, #c_var{name=V2}) -> #c_alias{var=#c_var{name=V2},pat=P1};
pat_alias(#c_cons{head=H1,tail=T1}, #c_cons{head=H2,tail=T2}) ->
    #c_cons{head=pat_alias(H1, H2), tail=pat_alias(T1, T2)};
pat_alias(#c_tuple{es=Es1}, #c_tuple{es=Es2}) ->
    #c_tuple{es=pat_alias_list(Es1, Es2)};
pat_alias(#c_alias{var=V1,pat=P1},
	   #c_alias{var=V2,pat=P2}) ->
    if V1 == V2 -> pat_alias(P1, P2);
       true -> #c_alias{var=V1,pat=#c_alias{var=V2,pat=pat_alias(P1, P2)}}
    end;
pat_alias(#c_alias{var=V1,pat=P1}, P2) ->
    #c_alias{var=V1,pat=pat_alias(P1, P2)};
pat_alias(P1, #c_alias{var=V2,pat=P2}) ->
    #c_alias{var=V2,pat=pat_alias(P1, P2)};
pat_alias(P, P) -> P.

%% pat_alias_list([A1], [A2]) -> [A].

pat_alias_list([A1|A1s], [A2|A2s]) ->
    [pat_alias(A1, A2)|pat_alias_list(A1s, A2s)];
pat_alias_list([], []) -> [].

%% pattern_list([P]) -> [P].

pattern_list(Ps) -> map(fun pattern/1, Ps).

%% first([A]) -> [A].
%% last([A]) -> A.

last([L]) -> L;
last([H|T]) -> last(T).

first([L]) -> [];
first([H|T]) -> [H|first(T)].

%% make_vars([Name]) -> [{Var,Name}].

make_vars(Vs) -> [ #c_var{name=V} || V <- Vs ].

%% new_var_name(State) -> {VarName,State}.

new_var_name(St) ->
    C = St#core.vcount,
    {list_to_atom("cor" ++ integer_to_list(C)),St#core{vcount=C + 1}}.

%% new_var(State) -> {{var,Name},State}.

new_var(St0) ->
    {New,St1} = new_var_name(St0),
    {#c_var{name=New},St1}.

%% new_vars(Count, State) -> {[Var],State}.
%%  Make Count new variables.

new_vars(N, St) -> new_vars(N, St, []).

new_vars(N, St0, Vs) when N > 0 ->
    {V,St1} = new_var(St0),
    new_vars(N-1, St1, [V|Vs]);
new_vars(0, St, Vs) -> {Vs,St}.

ubody(B, St) -> uexpr(B, [], St).

%% uclauses([Lclause], [KnownVar], State) -> {[Lclause],State}.

uclauses(Lcs, Ks, St0) ->
    mapfoldl(fun (Lc, St) -> uclause(Lc, Ks, St) end, St0, Lcs).

%% uclause(Lclause, [KnownVar], State) -> {Lclause,State}.

uclause(#iclause{pats=Ps0,guard=G0,body=B0}, Ks0, St0) ->
    {Ps1,Pg,Pvs,St1} = upattern_list(Ps0, Ks0, St0),
    Pu = intersection(Pvs, Ks0),
    Pn = subtract(Pvs, Pu),
    Ks1 = union(Pn, Ks0),
    {G1,St2} = uexprs(Pg ++ G0, Ks1, St1),
    Gu = used_in_any(G1),
    Gn = new_in_any(G1),			%This should be empty
    Ks2 = union(Gn, Ks1),
    {B1,St3} = uexprs(B0, Ks2, St2),
    Used = intersection(union([Pu,Gu,used_in_any(B1)]), Ks0),
    New = union([Pn,Gn,new_in_any(B1)]),
    {#iclause{anno=#a{us=Used,ns=New},pats=Ps1,guard=G1,body=B1},St3}.

uexprs([#imatch{pat=P0,arg=Arg,fc=Fc}|Les], Ks, St0) ->
    %% Optimise for simple set of unbound variable.
    case upattern(P0, Ks, St0) of
	{#c_var{},[],Pvs,St1} ->
	    %% Throw our work away and just set to iset.
	    uexprs([#iset{var=P0,arg=Arg}|Les], Ks, St0);
	Other ->
	    %% Throw our work away and set to icase.
	    if
		Les == [] ->
		    %% Need to explicitly return match "value", make
		    %% literal for efficiency.
		    {La,Lps,St1} = force_literal(Arg, St0),
		    Mc = #iclause{pats=[P0],guard=[],body=[La]},
		    uexprs(Lps ++ [#icase{args=[La],clauses=[Mc],fc=Fc}], Ks, St1);
		true ->
		    Mc = #iclause{pats=[P0],guard=[],body=Les},
		    uexprs([#icase{args=[Arg],clauses=[Mc],fc=Fc}], Ks, St0)
	    end
    end;
uexprs([Le0|Les0], Ks, St0) ->
    {Le1,St1} = uexpr(Le0, Ks, St0),
    {Les1,St2} = uexprs(Les0, union((get_ianno(Le1))#a.ns, Ks), St1),
    {[Le1|Les1],St2};
uexprs([], Ks, St) -> {[],St}.

uexpr(#iset{var=V,arg=A0}, Ks, St0) ->
    {A1,St1} = uexpr(A0, Ks, St0),
    {#iset{anno=#a{us=del_element(V#c_var.name, (get_ianno(A1))#a.us),
		   ns=add_element(V#c_var.name, (get_ianno(A1))#a.ns)},
	   var=V,arg=A1},St1};
%% imatch done in uexprs.
uexpr(#icase{args=As0,clauses=Cs0,fc=Fc0}, Ks, St0) ->
    %% As0 will never generate new variables.
    {As1,St1} = uexpr_list(As0, Ks, St0),
    {Cs1,St2} = uclauses(Cs0, Ks, St1),
    {Fc1,St3} = uclause(Fc0, Ks, St2),
    Used = union(used_in_any(As1), used_in_any(Cs1)),
    New = new_in_all(Cs1),
    {#icase{anno=#a{us=Used,ns=New},args=As1,clauses=Cs1,fc=Fc1},St3};
uexpr(#ifun{id=Id,vars=As,body=Es0}, Ks, St0) ->
    Avs = lit_list_vars(As),
    {Es1,St1} = uexprs(Es0, Ks, St0),
    Used = subtract(intersection(used_in_any(Es1), Ks), Avs),
    {#ifun{anno=#a{us=Used,ns=[]},id=Id,vars=As,body=Es1},St1};
uexpr(#icall{op=Op,args=As}, Ks, St) ->
    Used = union(lit_vars(Op), lit_list_vars(As)),
    {#icall{anno=#a{us=Used},op=Op,args=As},St};
uexpr(#icatch{body=Es0}, Ks, St0) ->
    {Es1,St1} = uexprs(Es0, Ks, St0),
    {#icatch{anno=#a{us=used_in_any(Es1)},body=Es1},St1};
uexpr(#ireceive1{clauses=Cs0}, Ks, St0) ->
    {Cs1,St1} = uclauses(Cs0, Ks, St0),
    {#ireceive1{anno=#a{us=used_in_any(Cs1),ns=new_in_all(Cs1)},
		clauses=Cs1},St1};
uexpr(#ireceive2{clauses=Cs0,timeout=Te0,action=Tes0}, Ks, St0) ->
    %% Te0 will never generate new variables.
    {Te1,St1} = uexpr(Te0, Ks, St0),
    {Cs1,St2} = uclauses(Cs0, Ks, St1),
    {Tes1,St3} = uexprs(Tes0, Ks, St2),
    Used = union([used_in_any(Cs1),used_in_any(Tes1),(get_ianno(Te1))#a.us]),
    New = intersection(new_in_all(Cs1), new_in_any(Tes1)),
    {#ireceive2{anno=#a{us=Used,ns=New},
		clauses=Cs1,timeout=Te1,action=Tes1},St2};
uexpr(Lit, Ks, St) ->
    true = is_lit(Lit),			%Sanity check!
    Vs = lit_vars(Lit),
    {set_ianno(Lit, #a{us=Vs}),St}.

uexpr_list(Les0, Ks, St0) ->
    mapfoldl(fun (Le, St) -> uexpr(Le, Ks, St) end, St0, Les0).

%% upattern(Pat, [KnownVar], State) -> {Pat,[GuardTest],[Var],State}.

upattern(#c_var{name='_'}, Ks, St0) ->
    {New,St1} = new_var_name(St0),
    {#c_var{name=New},[],[New],St1};
upattern(#c_var{name=V}, Ks, St0) ->
    case is_element(V, Ks) of
	true ->
	    {New,St1} = new_var_name(St0),
	    {#c_var{name=New},
	     [#icall{anno=#a{us=add_element(New, [V])},
		     op=#c_remote{mod=erlang,name='=:=',arity=2},
		     args=[#c_var{name=New},#c_var{name=V}]}],
	     [New],St1};
	false -> {#c_var{name=V},[],[V],St0}
    end;
upattern(#c_cons{head=H0,tail=T0}, Ks, St0) ->
    {H1,Hg,Hv,St1} = upattern(H0, Ks, St0),
    {T1,Tg,Tv,St2} = upattern(T0, union(Hv, Ks), St1),
    {#c_cons{head=H1,tail=T1},Hg ++ Tg,union(Hv, Tv),St2};
upattern(#c_tuple{es=Es0}, Ks, St0) ->
    {Es1,Esg,Esv,St1} = upattern_list(Es0, Ks, St0),
    {#c_tuple{es=Es1},Esg,Esv,St1};
upattern(#c_vector{es=Es0}, Ks, St0) ->
    {Es1,Esg,Esv,St1} = upattern_list(Es0, Ks, St0),
    {#c_vector{es=Es1},Esg,Esv,St1};
upattern(#c_alias{var=V0,pat=P0}, Ks, St0) ->
    {V1,Vg,Vv,St1} = upattern(V0, Ks, St0),
    {P1,Pg,Pv,St2} = upattern(P0, union(Vv, Ks), St1),
    {#c_alias{var=V1,pat=P1},Vg ++ Pg,union(Vv, Pv),St2};
upattern(Other, Ks, St) -> {Other,[],[],St}.	%Constants

%% upattern([Pat], [KnownVar], State) -> {[Pat],[GuardTest],[Var],State}.

upattern_list([P0|Ps0], Ks, St0) ->
    {P1,Pg,Pv,St1} = upattern(P0, Ks, St0),
    {Ps1,Psg,Psv,St2} = upattern_list(Ps0, union(Pv, Ks), St1),
    {[P1|Ps1], Pg ++ Psg,union(Pv, Psv),St2};
upattern_list([], Ks, St) -> {[],[],[],St}.    

used_in_any(Les) ->
    foldl(fun (Le, Ns) -> union((get_ianno(Le))#a.us, Ns) end, [], Les).

new_in_any(Les) ->
    foldl(fun (Le, Ns) -> union((get_ianno(Le))#a.ns, Ns) end, [], Les).

new_in_all([Le|Les]) ->
    foldl(fun (L, Ns) -> intersection((get_ianno(L))#a.ns, Ns) end,
	  (get_ianno(Le))#a.ns, Les);
new_in_all([]) -> [].

cbody(B0, St0) ->
    %%{B1,Es,As,St1} = cexpr(B0, [], St0),
    %%{B1,St1}.
    case catch cexpr(B0, [], St0) of
	{B1,Es,As,St1} -> {B1,St1};
	{'EXIT',R} -> {{R,B0},St0}
    end.

%% cclause(Lclause, [AfterVar], [ExpVar], State) -> {Cclause,State}.

cclause(#iclause{pats=Ps,guard=G0,body=B0}, As0, Exp, St0) ->
    {B1,As1,St1} = cexprs(B0, Exp, As0, St0),
    {G1,As2,St2} = cexprs(G0, [], As1, St1),
    {#c_clause{pat=make_values(Ps),guard=G1,body=B1},St2}.

cclauses(Lcs, As, Es, St0) ->
    mapfoldl(fun (Lc, St) -> cclause(Lc, As, Es, St) end, St0, Lcs).

%% cexprs([Lexpr], [ExpVar], [AfterVar], State) -> {Cexpr,[AfterVar],State}.
%%  Must be sneaky here at the last expr when combining exports for the
%%  whole sequence and exports for that expr.

cexprs([#iset{anno=A,var=V,arg=Arg}], Exp, As, St) ->
    %% Make return value explicit.
    cexprs([#iset{anno=A,var=V,arg=Arg},make_values(V, Exp)], [], As, St);
cexprs([Le], Exp, As0, St0) ->
    {Ce,Es,As1,St1} = cexpr(Le, As0, St0),
    if
	Es == [] -> {make_values(Ce, Exp),As1,St1};
	true ->
	    {R,St2} = new_var(St1),
	    {#c_let{vars=make_values(R, Es),arg=Ce,body=make_values(R, Exp)},As1,St2}
    end;
cexprs([#iset{var=V,arg=A0}|Les], Exp, As0, St0) ->
    {Ces,As1,St1} = cexprs(Les, Exp, As0, St0),
    {A1,Es,As2,St2} = cexpr(A0, As1, St1),
    {#c_let{vars=make_values(V, Es),arg=A1,body=Ces},As2,St2};
cexprs([Le|Les], Exp, As0, St0) ->
    {Ces,As1,St1} = cexprs(Les, Exp, As0, St0),
    {Ce,Es,As2,St2} = cexpr(Le, As1, St1),
    if
	Es == [] -> {#c_seq{arg=Ce,body=Ces},As2,St2};
	true ->
	    {R,St3} = new_var(St2),
	    {#c_let{vars=make_values(R, Es),arg=Ce,body=Ces},As2,St2}
    end;
cexprs([], Exp, As, St) ->			%Can only occur in guards.
    {#c_atom{name=true},As,St}.

%% cexpr(Lexpr, [AfterVar], State) -> {Cexpr,[ExpVar],[AfterVar],State}.

cexpr(#iset{anno=A,var=V,arg=A0}, As0, St0) ->
    {A1,Es,As1,St1} = cexpr(A0, As0, St0),
    {#c_let{vars=make_values(V, Es),arg=A1,body=V},[],As1,St1};
cexpr(#icase{anno=A,args=Largs,clauses=Lcs,fc=Lfc}, As, St0) ->
    Exp = make_vars(intersection(A#a.ns, As)),	%Exports
    {Cargs,St1} = foldr(fun (La, {Cas,Sta}) ->
				{Ca,[],As1,Stb} = cexpr(La, As, Sta),
				{[Ca|Cas],Stb}
			end, {[],St0}, Largs),
    {Ccs,St2} = cclauses(Lcs, As, Exp, St1),
    {Cfc,St3} = cclause(Lfc, As, [], St2),	%Never exports
    {#c_case{arg=make_values(Cargs),clauses=Ccs ++ [Cfc]},
     Exp,union(A#a.us, As),St3};
cexpr(#ireceive1{anno=A,clauses=Lcs}, As, St0) ->
    Exp = make_vars(intersection(A#a.ns, As)),	%Exports
    {Ccs,St1} = cclauses(Lcs, As, Exp, St0),
    {#c_receive{clauses=Ccs,
	       timeout=#c_atom{name=infinity},action=#c_atom{name=true}},
     Exp,union(A#a.us, As),St1};
cexpr(#ireceive2{anno=A,clauses=Lcs,timeout=Lto,action=Les}, As, St0) ->
    Exp = make_vars(intersection(A#a.ns, As)),	%Exports
    {Cto,[],As1,St1} = cexpr(Lto, As, St0),
    {Ccs,St2} = cclauses(Lcs, As, Exp, St1),
    {Ces,As2,St3} = cexprs(Les, Exp, As, St2),
    {#c_receive{clauses=Ccs,timeout=Cto,action=Ces},
     Exp,union(A#a.us, As),St3};
cexpr(#icatch{anno=A,body=Les}, As, St0) ->
    {Ces,As1,St1} = cexprs(Les, [], [], St0),	%Never export!
    {#c_catch{body=Ces},[],union(A#a.us, As),St1};
cexpr(#ifun{anno=A,id=Id,vars=Args,body=Les}, As, St0) ->
    {Ces,As1,St1} = cexprs(Les, [], [], St0),	%Never export!
    {#c_fun{anno=Id,vars=Args,body=Ces},[],union(A#a.us, As),St1};
cexpr(#icall{anno=A,op=Op,args=Args}, As, St) ->
    {#c_call{op=Op,args=Args},[],union(A#a.us, As),St};
cexpr(Lit, As, St) ->
    true = is_lit(Lit),			%Sanity check!
    Vs = lit_vars(Lit),
    {clear_ianno(Lit),[],union(Vs, As),St}.

%% lit_vars(Literal) -> [Var].

lit_vars(Lit) -> lit_vars(Lit, []).

lit_vars(#c_var{name=V}, Vs) -> add_element(V, Vs); 
lit_vars(#c_cons{head=H,tail=T}, Vs) -> lit_vars(H, lit_vars(T, Vs));
lit_vars(#c_tuple{es=Es}, Vs) -> lit_list_vars(Es, Vs);
lit_vars(#c_vector{es=Es}, Vs) -> lit_list_vars(Es, Vs);
lit_vars(Other, Vs) -> Vs.

lit_list_vars(Ls) -> lit_list_vars(Ls, []).

lit_list_vars(Ls, Vs) ->
    foldl(fun (L, Vs0) -> lit_vars(L, Vs0) end, Vs, Ls).

%% is_lit(Cexpr) -> bool().

is_lit(#c_int{}) -> true;
is_lit(#c_float{}) -> true;
is_lit(#c_atom{}) -> true;
is_lit(#c_char{}) -> true;
is_lit(#c_string{}) -> true;
is_lit(#c_var{}) -> true;
is_lit(#c_nil{}) -> true;
is_lit(#c_cons{head=H,tail=T}) ->
    case is_lit(H) of
	true -> is_lit(T);
	false -> false
    end;
is_lit(#c_tuple{es=Es}) -> is_lit_list(Es);
is_lit(#c_vector{es=Es}) -> is_lit_list(Es);
is_lit(Other) -> false.

is_lit_list([L|Ls]) ->
    case is_lit(L) of
	true -> is_lit_list(Ls); 
	false -> false
    end;
is_lit_list([]) -> true.

make_values(R, []) -> R;
make_values(R, Es) when list(Es) -> #c_vector{es=[R|Es]}.

make_values([P]) -> P;
make_values(Ps) when list(Ps) -> #c_vector{es=Ps};
make_values(P) -> P.

fail_clause(Pats, A) ->
    #iclause{pats=Pats,guard=[],
	     body=[#icall{op=#c_internal{name=match_fail,arity=1},args=[A]}]}.
