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
%% N.B. annotations fields in place as normal Core expressions.

-record(iset, {anno=[],var,arg}).
-record(imatch, {anno=[],pat,guard=[],arg,fc}).
-record(icase, {anno=[],args,clauses,fc}).
-record(iclause, {anno=[],pats,guard,body}).
-record(ifun, {anno=[],id,vars,clauses,fc}).
-record(icall, {anno=[],op,args}).
-record(icatch, {anno=[],body}).
-record(ireceive1, {anno=[],clauses}).
-record(ireceive2, {anno=[],clauses,timeout,action}).

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
    Fc = fail_clause(Ps, #c_tuple{es=[#c_atom{name=function_clause}|Ps]}),
    {#ifun{id=[],vars=Args,clauses=Cs1,fc=Fc},St3}.

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
expr({char,L,C}, St) -> {#c_char{val=C},[],St};
expr({string,L,S}, St) -> {#c_string{val=S},[],St};
expr({cons,L,H0,T0}, St0) ->
    {H1,Hps,St1} = literal(H0, St0),
    {T1,Tps,St2} = literal(T0, St1),
    {#c_cons{head=H1,tail=T1},Hps ++ Tps,St2};
expr({tuple,L,Es0}, St0) ->
    {Es1,Eps,St1} = lit_list(Es0, St0),
    {#c_tuple{es=Es1},Eps,St1};
expr({bin,L,Es0}, St0) ->
    {Es1,Eps,St1} = expr_bin(Es0, St0),
    {#c_bin{es=Es1},Eps,St1};
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

%% expr_bin([ArgExpr], St) -> {[Arg],[PreExpr],St}.
%%  Flatten the arguments of a bin. Do this straight left to right!

expr_bin(Es, St) ->
    foldr(fun (E, {Ces,Esp,St0}) ->
		  {Ce,Ep,St1} = bin_element(E, St0),
		  {[Ce|Ces],Ep ++ Esp,St1}
	  end, {[],[],St}, Es).

bin_element({bin_element,L,E0,Size0,Type}, St0) ->
    {E1,Eps,St1} = literal(E0, St0),
    {Size1,Eps2,St2} = literal(Size0, St1),
    {#c_bin_elem{val=E1,size=Size1,type=Type},Eps ++ Eps2,St2}.

%% fun_tq(Id, [Clauses], State) -> {Fun,[PreExp],State}.

fun_tq(Id, Cs0, St0) ->
    {Cs1,St1} = clauses(Cs0, St0),
    Arity = length((hd(Cs1))#iclause.pats),
    {Args,St2} = new_vars(Arity, St1),
    {Ps,St3} = new_vars(Arity, St2),		%Need new variables here
    Fc = fail_clause(Ps, #c_tuple{es=[#c_atom{name=function_clause}|Ps]}),
    Fun = #ifun{id=[{id,Id}],				%We KNOW!
		vars=Args,clauses=Cs1,fc=Fc},
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
    case core_lib:is_literal_top(Ce) of
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
pattern({char,L,C}) -> #c_char{val=C};
pattern({string,L,S}) -> #c_string{val=S};
pattern({nil,L}) -> #c_nil{};
pattern({cons,L,H,T}) ->
    #c_cons{head=pattern(H),tail=pattern(T)};
pattern({tuple,L,Ps}) ->
    #c_tuple{es=pattern_list(Ps)};
pattern({bin,L,Ps}) ->
    #c_bin{es=pat_bin(Ps)};
pattern({match,L,P1,P2}) ->
    pat_alias(pattern(P1), pattern(P2)).

%% bin_pattern_list([P]) -> [P].

pat_bin(Ps) -> map(fun pat_element/1, Ps).

pat_element({bin_element,L,Term,Size,Type}) ->
    #c_bin_elem{val=pattern(Term),size=pattern(Size),type=Type}.

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

fail_clause(Pats, A) ->
    #iclause{pats=Pats,guard=[],
	     body=[#icall{op=#c_internal{name=match_fail,arity=1},args=[A]}]}.

ubody(B, St) -> uexpr(B, [], St).

%% uclauses([Lclause], [KnownVar], State) -> {[Lclause],State}.

uclauses(Lcs, Ks, St0) ->
    mapfoldl(fun (Lc, St) -> uclause(Lc, Ks, St) end, St0, Lcs).

%% uclause(Lclause, [KnownVar], State) -> {Lclause,State}.

uclause(#iclause{pats=Ps0,guard=G0,body=B0}, Ks0, St0) ->
    {Ps1,Pg,Pvs,Pus,St1} = upattern_list(Ps0, Ks0, St0),
    Pu = union(Pus, intersection(Pvs, Ks0)),
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
	{#c_var{},[],Pvs,Pus,St1} ->
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
    {Les1,St2} = uexprs(Les0, union((core_lib:get_anno(Le1))#a.ns, Ks), St1),
    {[Le1|Les1],St2};
uexprs([], Ks, St) -> {[],St}.

uexpr(#iset{var=V,arg=A0}, Ks, St0) ->
    {A1,St1} = uexpr(A0, Ks, St0),
    {#iset{anno=#a{us=del_element(V#c_var.name, (core_lib:get_anno(A1))#a.us),
		   ns=add_element(V#c_var.name, (core_lib:get_anno(A1))#a.ns)},
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
uexpr(#ifun{id=Id,vars=As,clauses=Cs0,fc=Fc0}, Ks0, St0) ->
    Avs = lit_list_vars(As),
    Ks1 = union(Avs, Ks0),
    {Cs1,St1} = ufun_clauses(Cs0, Ks1, St0),
    {Fc1,St2} = ufun_clause(Fc0, Ks1, St1),
    Used = subtract(intersection(used_in_any(Cs1), Ks0), Avs),
    {#ifun{anno=#a{us=Used,ns=[]},id=Id,vars=As,clauses=Cs1,fc=Fc1},St2};
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
    Used = union([used_in_any(Cs1),used_in_any(Tes1),
		  (core_lib:get_anno(Te1))#a.us]),
    New = intersection(new_in_all(Cs1), new_in_any(Tes1)),
    {#ireceive2{anno=#a{us=Used,ns=New},
		clauses=Cs1,timeout=Te1,action=Tes1},St2};
uexpr(Lit, Ks, St) ->
    true = core_lib:is_literal(Lit),		%Sanity check!
    Vs = lit_vars(Lit),
    {core_lib:set_anno(Lit, #a{us=Vs}),St}.

uexpr_list(Les0, Ks, St0) ->
    mapfoldl(fun (Le, St) -> uexpr(Le, Ks, St) end, St0, Les0).

%% ufun_clauses([Lclause], [KnownVar], State) -> {[Lclause],State}.

ufun_clauses(Lcs, Ks, St0) ->
    mapfoldl(fun (Lc, St) -> ufun_clause(Lc, Ks, St) end, St0, Lcs).

%% ufun_clause(Lclause, [KnownVar], State) -> {Lclause,State}.

ufun_clause(#iclause{pats=Ps0,guard=G0,body=B0}, Ks0, St0) ->
    {Ps1,Pg,Pvs,Pus,St1} = upattern_list(Ps0, [], St0),
    Pu = union(Pus, intersection(Pvs, Ks0)),
    Pn = subtract(Pvs, Pu),
    Ks1 = union(Pn, Ks0),
    {G1,St2} = uexprs(Pg ++ G0, Ks1, St1),
    Gu = used_in_any(G1),
    Gn = new_in_any(G1),			%This should be empty
    Ks2 = union(Gn, Ks1),
    {B1,St3} = uexprs(B0, Ks2, St2),
    Used = subtract(intersection(union([Pu,Gu,used_in_any(B1)]), Ks0), Pvs),
    {#iclause{anno=#a{us=Used,ns=[]},pats=Ps1,guard=G1,body=B1},St3}.

%% upattern(Pat, [KnownVar], State) ->
%%              {Pat,[GuardTest],[NewVar],[UsedVar],State}.

upattern(#c_var{name='_'}, Ks, St0) ->
    {New,St1} = new_var_name(St0),
    {#c_var{name=New},[],[New],[],St1};
upattern(#c_var{name=V}, Ks, St0) ->
    case is_element(V, Ks) of
	true ->
	    {New,St1} = new_var_name(St0),
	    {#c_var{name=New},
	     [#icall{anno=#a{us=add_element(New, [V])},
		     op=#c_remote{mod=erlang,name='=:=',arity=2},
		     args=[#c_var{name=New},#c_var{name=V}]}],
	     [New],[],St1};
	false -> {#c_var{name=V},[],[V],[],St0}
    end;
upattern(#c_cons{head=H0,tail=T0}, Ks, St0) ->
    {H1,Hg,Hv,Hu,St1} = upattern(H0, Ks, St0),
    {T1,Tg,Tv,Tu,St2} = upattern(T0, union(Hv, Ks), St1),
    {#c_cons{head=H1,tail=T1},Hg ++ Tg,union(Hv, Tv),union(Hu, Tu),St2};
upattern(#c_tuple{es=Es0}, Ks, St0) ->
    {Es1,Esg,Esv,Eus,St1} = upattern_list(Es0, Ks, St0),
    {#c_tuple{es=Es1},Esg,Esv,Eus,St1};
upattern(#c_bin{es=Es0}, Ks, St0) ->
    {Es1,Esg,Esv,Eus,St1} = upat_bin(Es0, Ks, St0),
    {#c_bin{es=Es1},Esg,Esv,Eus,St1};
upattern(#c_alias{var=V0,pat=P0}, Ks, St0) ->
    {V1,Vg,Vv,Vu,St1} = upattern(V0, Ks, St0),
    {P1,Pg,Pv,Pu,St2} = upattern(P0, union(Vv, Ks), St1),
    {#c_alias{var=V1,pat=P1},Vg ++ Pg,union(Vv, Pv),union(Vu, Pu),St2};
upattern(Other, Ks, St) -> {Other,[],[],[],St}.	%Constants

%% upattern_list([Pat], [KnownVar], State) ->
%%                        {[Pat],[GuardTest],[NewVar],[UsedVar],State}.

upattern_list([P0|Ps0], Ks, St0) ->
    {P1,Pg,Pv,Pu,St1} = upattern(P0, Ks, St0),
    {Ps1,Psg,Psv,Psu,St2} = upattern_list(Ps0, union(Pv, Ks), St1),
    {[P1|Ps1],Pg ++ Psg,union(Pv, Psv),union(Pu, Psu),St2};
upattern_list([], Ks, St) -> {[],[],[],[],St}.    

%% upat_bin([Pat], [KnownVar], State) ->
%%                        {[Pat],[GuardTest],[NewVar],[UsedVar],State}.

upat_bin([P0|Ps0], Ks, St0) ->
    {P1,Pg,Pv,Pu,St1} = upat_element(P0, Ks, St0),
    {Ps1,Psg,Psv,Psu,St2} = upat_bin(Ps0, union(Pv, Ks), St1),
    {[P1|Ps1],Pg ++ Psg,union(Pv, Psv),union(Pu, Psu),St2};
upat_bin([], Ks, St) -> {[],[],[],[],St}.    

upat_element(#c_bin_elem{val=H0,size=Sz}=Cons, Ks, St0) ->
    {H1,Hg,Hv,[],St1} = upattern(H0, Ks, St0),
    Us = case Sz of
	     #c_var{name=Vname} -> [Vname];
	     Other -> []
	 end,
    {Cons#c_bin_elem{val=H1},Hg,Hv,Us,St1}.

used_in_any(Les) ->
    foldl(fun (Le, Ns) -> union((core_lib:get_anno(Le))#a.us, Ns) end,
	  [], Les).

new_in_any(Les) ->
    foldl(fun (Le, Ns) -> union((core_lib:get_anno(Le))#a.ns, Ns) end,
	  [], Les).

new_in_all([Le|Les]) ->
    foldl(fun (L, Ns) -> intersection((core_lib:get_anno(L))#a.ns, Ns) end,
	  (core_lib:get_anno(Le))#a.ns, Les);
new_in_all([]) -> [].

%% The AfterVars are the variables which are used afterwards.  We need
%% this to work out which variables are actually exported and used
%% from case/receive.  In subblocks/clauses the AfterVars of the block
%% are just the exported variables.

cbody(B0, St0) ->
    %%{B1,Es,As,St1} = cexpr(B0, [], St0),
    %%{B1,St1}.
    case catch cexpr(B0, [], St0) of
	{B1,Es,Us,St1} -> {B1,St1};
	{'EXIT',R} -> {{R,B0},St0}
    end.

%% cclause(Lclause, [AfterVar], State) -> {Cclause,State}.
%%  The AfterVars are the exported variables.

cclause(#iclause{pats=Ps,guard=G0,body=B0}, Exp, St0) ->
    {B1,Us1,St1} = cexprs(B0, Exp, St0),
    {G1,Ss2,St2} = cexprs(G0, [], St1),
    {#c_clause{pats=Ps,guard=G1,body=B1},St2}.

cclauses(Lcs, Es, St0) ->
    mapfoldl(fun (Lc, St) -> cclause(Lc, Es, St) end, St0, Lcs).

%% cexprs([Lexpr], [AfterVar], State) -> {Cexpr,[AfterVar],State}.
%%  Must be sneaky here at the last expr when combining exports for the
%%  whole sequence and exports for that expr.

cexprs([#iset{var=#c_var{name=Name}=Var}=Iset], As, St) ->
    %% Make return value explicit,, and make Var true top level.
    cexprs([Iset,Var#c_var{anno=#a{us=[Name]}}], As, St);
cexprs([Le], As, St0) ->
    {Ce,Es,Us,St1} = cexpr(Le, As, St0),
    Exp = make_vars(As),			%The export variables
    if
	Es == [] -> {core_lib:make_values([Ce|Exp]),union(Us, As),St1};
	true ->
	    {R,St2} = new_var(St1),
	    {#c_let{vars=[R|make_vars(Es)],arg=Ce,
		    body=core_lib:make_values([R|Exp])},
	     union(Us, As),St2}
    end;
cexprs([#iset{var=V,arg=A0}|Les], As0, St0) ->
    {Ces,As1,St1} = cexprs(Les, As0, St0),
    {A1,Es,Us,St2} = cexpr(A0, As1, St1),
    {#c_let{vars=[V|make_vars(Es)],arg=A1,body=Ces},
     union(Us, As1),St2};
cexprs([Le|Les], As0, St0) ->
    {Ces,As1,St1} = cexprs(Les, As0, St0),
    {Ce,Es,Us,St2} = cexpr(Le, As1, St1),
    if
	Es == [] -> {#c_seq{arg=Ce,body=Ces},union(Us, As1),St2};
	true ->
	    {R,St3} = new_var(St2),
	    {#c_let{vars=[R|make_vars(Es)],arg=Ce,body=Ces},
	     union(Us, As1),St3}
    end;
cexprs([], As, St) ->			%Can only occur in guards.
    {#c_atom{name=true},As,St}.

%% cexpr(Lexpr, [AfterVar], State) -> {Cexpr,[ExpVar],[UsedVar],State}.

cexpr(#icase{anno=A,args=Largs,clauses=Lcs,fc=Lfc}, As, St0) ->
    Exp = intersection(A#a.ns, As),		%Exports
    {Cargs,St1} = foldr(fun (La, {Cas,Sta}) ->
				{Ca,[],Us1,Stb} = cexpr(La, As, Sta),
				{[Ca|Cas],Stb}
			end, {[],St0}, Largs),
    {Ccs,St2} = cclauses(Lcs, Exp, St1),
    {Cfc,St3} = cclause(Lfc, [], St2),		%Never exports
    {#c_case{arg=core_lib:make_values(Cargs),clauses=Ccs ++ [Cfc]},
     Exp,A#a.us,St3};
cexpr(#ireceive1{anno=A,clauses=Lcs}, As, St0) ->
    Exp = intersection(A#a.ns, As),		%Exports
    {Ccs,St1} = cclauses(Lcs, Exp, St0),
    {#c_receive{clauses=Ccs,
	       timeout=#c_atom{name=infinity},action=#c_atom{name=true}},
     Exp,A#a.us,St1};
cexpr(#ireceive2{anno=A,clauses=Lcs,timeout=Lto,action=Les}, As, St0) ->
    Exp = intersection(A#a.ns, As),		%Exports
    {Cto,[],Us1,St1} = cexpr(Lto, As, St0),
    {Ccs,St2} = cclauses(Lcs, Exp, St1),
    {Ces,Us2,St3} = cexprs(Les, Exp, St2),
    {#c_receive{clauses=Ccs,timeout=Cto,action=Ces},
     Exp,A#a.us,St3};
cexpr(#icatch{anno=A,body=Les}, As, St0) ->
    {Ces,Us1,St1} = cexprs(Les, [], St0),	%Never export!
    {#c_catch{body=Ces},[],A#a.us,St1};
cexpr(#ifun{anno=A,id=Id,vars=Args,clauses=Lcs,fc=Lfc}, As, St0) ->
    {Ccs,St1} = cclauses(Lcs, [], St0),		%NEVER export!
    {Cfc,St2} = cclause(Lfc, [], St1),
    {#c_fun{anno=Id,vars=Args,
	    body=#c_case{arg=core_lib:make_values(Args),clauses=Ccs ++ [Cfc]}},
     [],A#a.us,St2};
cexpr(#icall{anno=A,op=Op,args=Args}, As, St) ->
    {#c_call{op=Op,args=Args},[],A#a.us,St};
cexpr(Lit, As, St) ->
    true = core_lib:is_literal(Lit),		%Sanity check!
    Vs = (core_lib:get_anno(Lit))#a.us,
    %%Vs = lit_vars(Lit),
    {core_lib:set_anno(Lit, []),[],Vs,St}.

%% lit_vars(Literal) -> [Var].

lit_vars(Lit) -> lit_vars(Lit, []).

lit_vars(#c_var{name=V}, Vs) -> add_element(V, Vs); 
lit_vars(#c_cons{head=H,tail=T}, Vs) -> lit_vars(H, lit_vars(T, Vs));
lit_vars(#c_tuple{es=Es}, Vs) -> lit_list_vars(Es, Vs);
lit_vars(Other, Vs) -> Vs.

lit_list_vars(Ls) -> lit_list_vars(Ls, []).

lit_list_vars(Ls, Vs) ->
    foldl(fun (L, Vs0) -> lit_vars(L, Vs0) end, Vs, Ls).
