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
%% Purpose : Expand some source Erlang constructions. This is part of the
%%           pre-processing phase.

%% N.B. Although structs (tagged tuples) are not yet allowed in the
%% language there is code included in pattern/2 and expr/3 (commented out)
%% that handles them by transforming them to tuples.

-module(sys_pre_expand).

%% Main entry point.
-export([module/2]).

-import(ordsets, [list_to_set/1,add_element/2,
		  union/1,union/2,intersection/1,intersection/2,subtract/2]).
-import(lists,   [member/2,map/2,foldl/3,foldr/3]).

-record(expand, {module=[],			%Module name
		 exports=[],			%Exports
		 imports=[],			%Imports
		 compile=[],			%Compile flags
		 records=dict:new(),		%Record definitions
		 attributes=[],			%Attributes
		 defined=[],			%Defined functions
		 vcount=0,			%Variable counter
		 func=[]}).			%Current function

%% module(Forms, CompileOptions)
%%	{ModuleName,Exports,TransformedForms}
%%  Expand the forms in one module. N.B. the lists of predefined functions
%%  and exports are really ordsets!

module(Fs, Opts) ->
    %% Set pre-defined functions.
    PreDef = [{module_info,0},{module_info,1}],

    %% Set pre-defined exports.
    PreExp = [{module_info,0},{module_info,1}],

    %% Build initial expand record.
    St0 = #expand{exports=PreExp,
		  compile=Opts,
		  defined=PreDef},
    %% Expand the functions.
    {Tfs,St1} = forms(Fs, foldl(fun define_function/2, St0, Fs)),
    %% Get the correct list of exported functions.
    Exports = case member(export_all, St1#expand.compile) of
		  true -> St1#expand.defined;
		  false -> St1#expand.exports
	      end,
    %% Generate compile time info.
    {{Y,Mo,D}, {H,Mi,S}} = erlang:universaltime(),
    Compile = [{time,{Y,Mo,D,H,Mi,S}},{options,St1#expand.compile}],
    %% Generate all functions from stored info.
    {Ats,St2} = module_attrs(Exports, Compile, St1),
    {Mfs,St3} = module_info_funcs(Exports, Compile, St2),
    {St3#expand.module,Exports,Ats ++ Tfs ++ Mfs,St3#expand.compile}.

%% -type define_function(Form, State) -> State.
%%  Add function to defined if form a function.

define_function({attribute,L,asm,{function,N,A,Code}}, St) ->
    St#expand{defined=add_element({N,A}, St#expand.defined)};
define_function({function,L,N,A,Cs}, St) ->
    St#expand{defined=add_element({N,A}, St#expand.defined)};
define_function(F, St) -> St.

module_attrs(Exp, Compile, St) ->
    {[{attribute,0,Name,Val} || {Name,Val} <- St#expand.attributes],St}.

module_info_funcs(Exp, Compile, St) ->
    {[{function,0,module_info,0,
       [{clause,0,[],[], [{nil,0}]}]},
      {function,0,module_info,1,
       [{clause,0,[{var,0,'_'}],[], [{nil,0}]}]}],
     St}.

%% forms(Forms, State) ->
%%	{TransformedForms,State'}
%%  Process the forms. Attributes are lost and just affect the state.
%%  Asm things are funny, just pass them through. Ignore uninteresting forms
%%  like eof and type.

forms([{attribute,L,asm,{function,N,A,Code}}|Fs0], St0) ->
    {Fs,St1} = forms(Fs0, St0),
    {[{asm,N,A,Code}|Fs],St1};
forms([{attribute,L,Name,Val}|Fs0], St0) ->
    St1 = attribute(L, Name, Val, St0),
    forms(Fs0, St1);
forms([{function,L,N,A,Cs}|Fs0], St0) ->
    {Ff,St1} = function(L, N, A, Cs, St0),
    {Fs,St2} = forms(Fs0, St1),
    {[Ff|Fs],St2};
forms([F|Fs], St) -> forms(Fs, St);
forms([], St) -> {[],St}.

%% -type attribute(Line, Attribute, Value, State) ->
%%	State.
%%  Process an attribute, this just affects the state.

attribute(L, module, Module, St) ->
    St#expand{module=Module};
attribute(L, export, Es, St) ->
    St#expand{exports=union(list_to_set(Es), St#expand.exports)};
attribute(L, import, Is, St) ->
    import(L, Is, St);
attribute(L, compile, C, St) when list(C) ->
    St#expand{compile=St#expand.compile ++ C};
attribute(L, compile, C, St) ->
    St#expand{compile=St#expand.compile ++ [C]};
attribute(L, record, {Name,Defs}, St) ->
    St#expand{records=dict:store(Name, normalise_fields(Defs),
				 St#expand.records)};
attribute(L, file, File, St) ->	St;		%This is ignored
attribute(L, Name, Val, St) when list(Val) ->
    St#expand{attributes=St#expand.attributes ++ [{Name,Val}]};
attribute(L, Name, Val, St) ->
    St#expand{attributes=St#expand.attributes ++ [{Name,[Val]}]}.

function(L, N, A, Cs0, St0) ->
    {Cs,St} = clauses(Cs0, St0#expand{func=N}),
    {{function,L,N,A,Cs},St}.

%% -type clauses([Clause], State) ->
%%	{[TransformedClause],State}.
%%  Expand function clauses.

clauses([{clause,Line,H0,G0,B0}|Cs0], St0) ->
    {H,Hvs,St1} = head(H0, St0),
    {G,Gvs,Gus,St2} = guard(G0, Hvs, St1),
    {B,Bvs,Bus,St3} = exprs(B0, union(Hvs, Gvs), St2),
    {Cs,St4} = clauses(Cs0, St3),
    {[{clause,Line,H,G,B}|Cs],St4};
clauses([], St) -> {[],St}.

%% head(HeadPatterns, State) ->
%%	{TransformedPatterns,Variables,State'}

head(As, St) -> pattern_list(As, St).

%% pattern(Pattern, State) ->
%%	{TransformedPattern,Variables,State'}

string_to_conses(Line, Cs, Tail) ->
    foldr(fun (C, T) -> {cons,Line,{integer,Line,C},T} end, Tail, Cs).

pattern({var,Line,'_'}, St) ->			%Ignore anonymous variable.
    {{var,Line,'_'},[],St};
pattern({var,Line,V}, St) ->
    {{var,Line,V},[V],St};
pattern({integer,Line,I}, St) ->
    {{integer,Line,I},[],St};
pattern({float,Line,F}, St) ->
    {{float,Line,F},[],St};
pattern({atom,Line,A}, St) ->
    {{atom,Line,A},[],St};
pattern({string,Line,S}, St) ->
    {{string,Line,S},[],St};
pattern({nil,Line}, St) ->
    {{nil,Line},[],St};
pattern({cons,Line,H,T}, St0) ->
    {TH,THvs,St1} = pattern(H, St0),
    {TT,TTvs,St2} = pattern(T, St1),
    {{cons,Line,TH,TT},union(THvs, TTvs),St2};
pattern({tuple,Line,Ps}, St0) ->
    {TPs,TPsvs,St1} = pattern_list(Ps, St0),
    {{tuple,Line,TPs},TPsvs,St1};
%%pattern({struct,Line,Tag,Ps}, St0) ->
%%    {TPs,TPsvs,St1} = pattern_list(Ps, St0),
%%    {{tuple,Line,[{atom,Line,Tag}|TPs]},TPsvs,St1};
pattern({record_index,Line,Name,Field}, St) ->
    {index_expr(Line, Field, Name, record_fields(Name, St)),[],St};
pattern({record,Line,Name,Pfs}, St0) ->
    Fs = record_fields(Name, St0),
    {TMs,TMsvs,St1} = pattern_list(pattern_fields(Fs, Pfs), St0),
    {{tuple,Line,[{atom,Line,Name}|TMs]},TMsvs,St1};
pattern({op,Line,'++',{nil,_},R}, St) ->
    pattern(R, St);
pattern({op,Line,'++',{cons,Li,H,T},R}, St) ->
    pattern({cons,Li,H,{op,Li,'++',T,R}}, St);
pattern({op,Line,'++',{string,Li,L},R}, St) ->
    pattern(string_to_conses(Li, L, R), St);
pattern({match,Line,Pat1, Pat2}, St0) ->
    {TH,Hvt,St1} = pattern(Pat2, St0),
    {TT,Tvt,St2} = pattern(Pat1, St1),
    {{match,Line,TT,TH}, union(Hvt,Tvt), St2};
%% The following are necessary to be able to handle unary +,- in patterns.
pattern({op,Line,'+',{integer,Li,I}}, St) ->
    {{integer,Li,I},[],St};
pattern({op,Line,'-',{integer,Li,I}}, St) ->
    {{integer,Li,-I},[],St};
pattern({op,Line,'+',{float,Lf,F}}, St) ->
    {{float,Lf,F},[],St};
pattern({op,Line,'-',{float,Lf,F}}, St) ->
    {{float,Lf,-F},[],St}.

pattern_list([P|Ps], St0) ->
    {TP,TPvs,St1} = pattern(P, St0),
    {TPs,TPsvs,St2} = pattern_list(Ps, St1),
    {[TP|TPs],union(TPvs, TPsvs),St2};
pattern_list([], St) -> {[],[],St}.

%% guard(Guard, VisibleVariables, State) ->
%%	{TransformedGuard,NewVariables,UsedVariables,State'}
%%  Transform a list of guard tests. We KNOW that this has been checked
%%  and what the guards test are. Use expr for transforming the guard
%%  expressions.

guard([G0|Gs0], Vs, St0) ->
    {G,Hvs,Hus,St1} = guard_tests(G0, Vs, St0),
    {Gs,Tvs,Tus,St2} = guard(Gs0, Vs, St1),
    {[G|Gs],union(Hvs, Tvs),union(Hus, Tus),St2};
guard([], Vs, St) -> {[],[],[],St}.

guard_tests([{op,Line,Op,L0,R0}|Gs0], Vs, St0) ->
    {L,Lvs,Lus,St1} = expr(L0, Vs, St0),
    {R,Rvs,Rus,St2} = expr(R0, Vs, St1),
    {Gs,Gsvs,Gsus,St3} = guard_tests(Gs0, union(Lvs, Rvs), St2),
    {[{op,Line,Op,L,R}|Gs],union([Lvs,Rvs,Gsvs]),union([Lus,Rus,Gsus]),St3};
guard_tests([{call,Line,{atom,Lr,record},[A,{atom,Ln,Name}]}|Gs], Vs, St) ->
    Fs = record_fields(Name, St),
    guard_tests([{op,Line,'==',{call,Line,{atom,Ln,size},[A]},
		  {integer,Ln,length(Fs)+1}},
		 {op,Line,'==',{call,Line,{atom,Ln,element},[{integer,Ln,1},A]},
		  {atom,Ln,Name}}|Gs], Vs, St);
guard_tests([{call,Line,Test,As0}|Gs0], Vs, St0) ->
    {As,Asvs,Asus,St1} = expr_list(As0, Vs, St0),
    {Gs,Gsvs,Gsus,St2} = guard_tests(Gs0, union(Asvs, Vs), St1),
    TestF = {remote,Line,{atom,Line,erlang},Test},
    {[{call,Line,TestF,As}|Gs],union(Asvs, Gsvs),union(Asus, Gsus),St2};
guard_tests([{atom,Line,true}|Gs], Vs, St) ->
    guard_tests(Gs, Vs, St);
guard_tests([], Vs, St) ->
    {[],[],[],St}.

%% exprs(Expressions, VisibleVariables, State) ->
%%	{TransformedExprs,NewVariables,UsedVariables,State'}

exprs([E0|Es0], Vs, St0) ->
    {E,Evs,Eus,St1} = expr(E0, Vs, St0),
    {Es,Esvs,Esus,St2} = exprs(Es0, union(Evs, Vs), St1),
    {[E|Es],union(Evs, Esvs),union(Eus, Esus),St2};
exprs([], Vs, St) ->
    {[],[],[],St}.

%% expr(Expression, VisibleVariables, State) ->
%%	{TransformedExpression,NewVariables,UsedVariables,State'}

expr({var,Line,V}, Vs, St) ->
    {{var,Line,V},[],[V],St};
expr({integer,Line,I}, Vs, St) ->
    {{integer,Line,I},[],[],St};
expr({float,Line,F}, Vs, St) ->
    {{float,Line,F},[],[],St};
expr({atom,Line,A}, Vs, St) ->
    {{atom,Line,A},[],[],St};
expr({string,Line,S}, Vs, St) ->
    {{string,Line,S},[],[],St};
expr({nil,Line}, Vs, St) ->
    {{nil,Line},[],[],St};
expr({cons,Line,H0,T0}, Vs, St0) ->
    {H,Hvs,Hus,St1} = expr(H0, Vs, St0),
    {T,Tvs,Tus,St2} = expr(T0, Vs, St1),
    {{cons,Line,H,T},union(Hvs, Tvs),union(Hus, Tus),St2};
expr({lc,Line,E,Qs}, Vs, St) ->
    lc_tq(Line, E, Qs, {nil,Line}, Vs, St);
expr({tuple,Line,Es0}, Vs, St0) ->
    {Es1,Esvs,Esus,St1} = expr_list(Es0, Vs, St0),
    {{tuple,Line,Es1},Esvs,Esus,St1};
%%expr({struct,Line,Tag,Es0}, Vs, St0) ->
%%    {Es1,Esvs,Esus,St1} = expr_list(Es0, Vs, St0),
%%    {{tuple,Line,[{atom,Line,Tag}|Es1]},Esvs,Esus,St1};
expr({record_index,Line,Name,F}, Vs, St) ->
    I = index_expr(Line, F, Name, record_fields(Name, St)),
    expr(I, Vs, St);
expr({record,Line,Name,Is}, Vs, St) ->
    expr({tuple,Line,[{atom,Line,Name}|
		      record_inits(record_fields(Name, St), Is)]},
	 Vs, St);
expr({record_field,Line,R,Name,F}, Vs, St) ->
    I = index_expr(Line, F, Name, record_fields(Name, St)),
    expr({call,Line,{atom,Line,element},[I,R]}, Vs, St);
expr({record,Line,R,Name,Us}, Vs, St0) ->
    {Ue,St1} = record_update(R, Name, record_fields(Name, St0), Us, St0),
    expr(Ue, Vs, St1);
expr({block,Line,Es0}, Vs, St0) ->
    {Es,Esvs,Esus,St1} = exprs(Es0, Vs, St0),
    {{block,Line,Es},Esvs,Esus,St1};
expr({'if',Line,Cs0}, Vs, St0) ->
    {Cs,Csvss,Csuss,St1} = icr_clauses(Cs0, Vs, St0),
    {All,Some} = new_in(Vs, Csvss),
    {{'if',Line,Cs},All,union(Csuss),St1};
expr({'case',Line,E0,Cs0}, Vs, St0) ->
    {E,Evs,Eus,St1} = expr(E0, Vs, St0),
    {Cs,Csvss,Csuss,St2} = icr_clauses(Cs0, union(Evs, Vs), St1),
    {All,Some} = new_in(Vs, Csvss),
    {{'case',Line,E,Cs},union(Evs, All),union([Eus|Csuss]),St2};
expr({'receive',Line,Cs0}, Vs, St0) ->
    {Cs,Csvss,Csuss,St1} = icr_clauses(Cs0, Vs, St0),
    {All,Some} = new_in(Vs, Csvss),
    {{'receive',Line,Cs},All,union(Csuss),St1};
expr({'receive',Line,Cs0,To0,ToEs0}, Vs, St0) ->
    {To,Tovs,Tous,St1} = expr(To0, Vs, St0),
    {ToEs,ToEsvs,ToEsus,St2} = exprs(ToEs0, Vs, St1),
    {Cs,Csvss,Csuss,St3} = icr_clauses(Cs0, Vs, St2),
    {All,Some} = new_in(Vs, [ToEsvs|Csvss]),
    {{'receive',Line,Cs,To,ToEs},union(Tovs, All),union([Tous|Csuss]),St3};
expr({'fun',Line,Body}, Vs, St) ->
    fun_tq(Line, Body, Vs, St);
expr({call,Line,{atom,La,apply},As0}, Vs, St0) when length(As0) == 2 ->
    {As,Asvs,Asus,St1} = expr_list(As0, Vs, St0),
    {{call,Line,{remote,La,{atom,La,erlang},{atom,La,apply}},As},Asvs,Asus,St1};
expr({call,Line,{atom,La,record_info},[{atom,Li,Info},{atom,Ln,Name}]}, Vs, St) ->
    case Info of
	size -> {{integer,Line,1+length(record_fields(Name, St))},[],[],St};
	fields -> {make_list(field_names(record_fields(Name, St)), Line),
		   [],[],St}
    end;
expr({call,Line,{atom,La,N},As0}, Vs, St0) ->
    {As,Asvs,Asus,St1} = expr_list(As0, Vs, St0),
    Ar = length(As),
    case erl_internal:bif(N, Ar) of
	true ->
	    {{call,Line,{remote,La,{atom,La,erlang},{atom,La,N}},As},
	     Asvs,Asus,St1};
	false ->
	    case imported(N, Ar, St1) of
		{yes,Mod} ->
		    {{call,Line,{remote,La,{atom,La,Mod},{atom,La,N}},As},
		     Asvs,Asus,St1};
		no ->
		    {{call,Line,{atom,La,N},As},Asvs,Asus,St1}
	    end
    end;
expr({call,Line,{remote,Lr,{atom,Lm,M},{atom,Lf,F}},As0}, Vs, St0) ->
    {As1,Asvs,Asus,St1} = expr_list(As0, Vs, St0),
    {{call,Line,{remote,Lr,{atom,Lm,M},{atom,Lf,F}},As1},Asvs,Asus,St1};
expr({call,Line,{remote,Lr,M,F},As}, Vs, St) ->
    expr({call,Line,{atom,Line,apply},[M,F,make_list(As, Line)]}, Vs, St);
expr({call,Line,F,As0}, Vs, St0) ->
    {[Fun1|As1],Asvs,Asus,St1} = expr_list([F|As0], Vs, St0),
    {{call,Line,Fun1,As1},Asvs,Asus,St1};
expr({'catch',Line,E0}, Vs, St0) ->
    %% Catch exports no new variables.
    {E,Evs,Eus,St1} = expr(E0, Vs, St0),
    {{'catch',Line,E},[],Eus,St1};
expr({match,Line,P0,E0}, Vs, St0) ->
    {E,Evs,Eus,St1} = expr(E0, Vs, St0),
    {P,Pvs,St2} = pattern(P0, St1),
    {{match,Line,P,E},
     union(subtract(Pvs, Vs), Evs),
     union(intersection(Pvs, Vs), Eus),St2};
expr({op,Line,'++',{lc,Ll,E,Qs},L2}, Vs, St) ->
    lc_tq(Ll, E, Qs, L2, Vs, St);
expr({op,Ll,'++',{string,L1,S1},{string,L2,S2}}, Vs, St) ->
    {{string,L1,S1 ++ S2},[],[],St};
expr({op,Ll,'++',{string,L1,S1},R0}, Vs, St0) ->
    {R1,Rvs,Rus,St1} = expr(R0, Vs, St0),
    E = case R1 of
	    {string,L2,S2} -> {string,L1,S1 ++ S2};
	    Other -> string_to_conses(L1, S1, R1)
    end,
    {E,Rvs,Rus,St1};
expr({op,Ll,'++',{cons,Lc,H,T},L2}, Vs, St) ->
    expr({cons,Ll,H,{op,Lc,'++',T,L2}}, Vs, St);
expr({op,Ll,'++',{nil,Ln},L2}, Vs, St) ->
    expr(L2, Vs, St);
expr({op,Line,Op,A0}, Vs, St0) ->
    {A,Avs,Aus,St1} = expr(A0, Vs, St0),
    {{op,Line,Op,A},Avs,Aus,St1};
expr({op,Line,Op,L0,R0}, Vs, St0) ->
    {L,Lvs,Lus,St1} = expr(L0, Vs, St0),
    {R,Rvs,Rus,St2} = expr(R0, Vs, St1),
    {{op,Line,Op,L,R},union(Lvs, Rvs),union(Lus, Rus),St2}.

expr_list([E0|Es0], Vs, St0) ->
    {E,Evs,Eus,St1} = expr(E0, Vs, St0),
    {Es,Esvs,Esus,St2} = expr_list(Es0, Vs, St1),
    {[E|Es],union(Evs, Esvs),union(Eus, Esus),St2};
expr_list([], Vs, St) ->
    {[],[],[],St}.

%% icr_clauses([Clause], [VisibleVariable], State) ->
%%	{[TransformedClause],[[NewVariable]],[[UsedVariable]],State'}
%%  Be very careful here to return the variables that are really used
%%  and really new.

icr_clauses([{clause,Line,H0,G0,B0}|Cs0], Vs, St0) ->
    {H,Hvs,St1} = head(H0, St0),		%Hvs is really used!
    {G,Gvs,Gus,St2} = guard(G0, union(Hvs, Vs), St1),
    {B,Bvs,Bus,St3} = exprs(B0, union([Vs,Hvs,Gvs]), St2),
    New = subtract(union([Hvs,Gvs,Bvs]), Vs),	%Really new
    Used = intersection(union([Hvs,Gus,Bus]), Vs), %Really used
    {Cs,Csvs,Csus,St4} = icr_clauses(Cs0, Vs, St3),
    {[{clause,Line,H,G,B}|Cs],[New|Csvs],[Used|Csus],St4};
icr_clauses([], Vs, St) ->
    {[],[],[],St}.

%% lc_tq(Line, Expr, Qualifiers, More, [VisibleVar], State) ->
%%	{ListComp,[NewVar],[UsedVar],State'}
%%
%% This TQ from Simon PJ pp 127-138.  No need to call ourselves
%% recursively on append as this is special cased in expr as an
%% optimisation. As Erlang doesn't have letrec's we explicitly pass
%% the lambda as an extra argument to be able to do the recursive
%% call. We also do the argument matching in the head of the lambda
%% instead of in a 'case' so as to automatically get the correct
%% shadowing.

lc_tq(Line, E, Qs, More, Vs, St0) ->
    {Lc,St1} = lc_tq(Line, E, Qs, More, St0),
    expr(Lc, Vs, St1).

lc_tq(Line, E, [{generate,Lg,P,G}|Qs], More, St0) ->
    {Fun,St1} = new_var(Lg, St0),
    {Tail,St2} = new_var(Lg, St1),
    NewMore = {call,Lg,Fun,[Tail,Fun]},
    {Lc,St3} = lc_tq(Line, E, Qs, NewMore, St2),
    {{block,Lg,
      [{match,Lg,Fun,
	{'fun',Lg,
	 {clauses,[{clause,Lg,[{cons,Lg,P,Tail},Fun],[],[Lc]},
		   {clause,Lg,[{cons,Lg,{var,Lg,'_'},Tail},Fun],[],[NewMore]},
		   {clause,Lg,[{nil,Lg},Fun],[],[More]}]}}},
       {call,Lg,Fun,[G,Fun]}]},
     St3};
lc_tq(Line, E, [F|Qs], More, St0) ->
    Lf = element(2, F),
    {Lc,St1} = lc_tq(Line, E, Qs, More, St0),
    case erl_lint:is_guard_test(F) of
	true ->
	    {{'if',Lf,
	      [{clause,Lf,[],[[F]],[Lc]},
	       {clause,Lf,[],[],[More]}]},St1};
	false ->
	    case F of
		{op,_,'not',F1} ->		%Get rid of not operator.
		    {{'case',Lf,F1,
		      [{clause,Lf,[{atom,Lf,false}],[],[Lc]},
		       {clause,Lf,[{atom,Lf,true}],[],[More]}]},St1};
		Other ->
		    {{'case',Lf,F,
		      [{clause,Lf,[{atom,Lf,true}],[],[Lc]},
		       {clause,Lf,[{atom,Lf,false}],[],[More]}]},St1}
	    end
    end;
lc_tq(Line, E, [], More, St) ->
    {{cons,Line,E,More},St}.

%% fun_tq(Line, Body, VisibleVariables, State) ->
%%	{Fun,NewVariables,UsedVariables,State'}
%%  Transform a fun into into a tuple {'fun',Mod,I,Uniq,Free} and add
%%  clauses to module_lambdas/4 to hold the code. Process the body
%%  sequence directly to get the new and used variables, this also
%%  allows recursives call for the function case to correctly handle
%%  BIFs. N.B. erl_lint disallows imports so we don't have to check.

fun_tq(Lf, {function,F,A}, Vs, St0) ->
    %% Core {{'fun',Lf,{function,F,A}},[],[],St0}
    {As,St1} = new_vars(A, Lf, St0),
    fun_tq(Lf, {clauses,[{clause,Lf,As,[],[{call,Lf,{atom,Lf,F},As}]}]},
	   Vs, St1);
fun_tq(Lf, {clauses,Cs0}, Vs, St0) ->
    Uniq = erlang:hash(Cs0, (1 bsl 27)-1),
    %% The added clauses are transformed directly to get
    %% new/used vars.  The line number hack is to get the same
    %% indentifier in both cases.  This SUCKS!
    {Cs1,Hvss,Frees,St1} = fun_clauses(Cs0, Vs, St0),
    Ufrees = union(Frees),
    Free = intersection(Ufrees, Vs),
    {{'fun',Lf,{clauses,Cs1},{Uniq,Hvss,Free}},[],Ufrees,St1}.

fun_clauses([{clause,L,H0,G0,B0}|Cs0], Vs, St0) ->
    {H,Hvs,St1} = head(H0, St0),
    {G,Gvs,Gus,St2} = guard(G0, union(Hvs, Vs), St1),
    {B,Bvs,Bus,St3} = exprs(B0, union([Vs,Hvs,Gvs]), St2),
    %% Free variables cannot be new anywhere in the clause.
    Free = subtract(union(Gus, Bus), union([Hvs,Gvs,Bvs])),
    %%io:format(" Gus :~p~n Bvs :~p~n Bus :~p~n Free:~p~n" ,[Gus,Bvs,Bus,Free]),
    {Cs,Hvss,Frees,St4} = fun_clauses(Cs0, Vs, St3),
    {[{clause,L,H,G,B}|Cs],[Hvs|Hvss],[Free|Frees],St4};
fun_clauses([], Vs, St) -> {[],[],[],St}.


%% normalise_fields([RecDef]) -> [Field].
%%  Normalise the field definitions to always have a default value. If
%%  none has been given then use 'undefined'.

normalise_fields(Fs) ->
    map(fun ({record_field,Lf,Field}) ->
		{record_field,Lf,Field,{atom,Lf,undefined}};
	    (F) -> F end, Fs).

%% record_fields(RecordName, State)
%% find_field(FieldName, Fields)

record_fields(R, St) -> dict:fetch(R, St#expand.records).

find_field(F, [{record_field,Lf,{atom,La,F},Val}|Fs]) -> {ok,Val};
find_field(F, [_|Fs]) -> find_field(F, Fs);
find_field(F, []) -> error.

%% field_names(RecFields) -> [Name].
%%  Return a list of the field names structures.

field_names(Fs) ->
    map(fun ({record_field,Lf,Field,Val}) -> Field end, Fs).

%% index_expr(Line, FieldExpr, Name, Fields) -> IndexExpr.
%%  Return an expression which evaluates to the index of a
%%  field. Currently only handle the case where the field is an
%%  atom. This expansion must be passed through expr again.

index_expr(Line, {atom,La,F}, Name, Fs) ->
    {integer,Line,index_expr(F, Fs, 2)}.

index_expr(F, [{record_field,Lf,{atom,La,F},Val}|Fs], I) -> I;
index_expr(F, [_|Fs], I) ->
    index_expr(F, Fs, I+1).

%% pattern_fields([RecDefField], [Match]) -> [Pattern].
%%  Build a list of match patterns for the record tuple elements.
%%  This expansion must be passed through pattern again. N.B. We are
%%  scanning the record definition field list!

pattern_fields(Fs, Ms) ->
    map(fun ({record_field,L,{atom,La,F},D}) ->
		case find_field(F, Ms) of
		    {ok,Match} -> Match;
		    error -> {var,L,'_'}
		end end,
	Fs).

%% record_inits([RecDefField], [Init]) -> [InitExpr].
%%  Build a list of initialisation expressions for the record tuple
%%  elements. This expansion must be passed through expr
%%  again. N.B. We are scanning the record definition field list!

record_inits(Fs, Is) ->
    map(fun ({record_field,L,{atom,La,F},D}) ->
		case find_field(F, Is) of
		    {ok,Init} -> Init;
		    error -> D
		end end,
	Fs).

%% record_update(Record, RecordName, [RecDefField], [Update], State) ->
%%	{Expr,State'}
%%  Build an expression to update fields in a record returning a new
%%  record.  Try to be smart and optimise this. This expansion must be
%%  passed through expr again.

record_update(R, Name, Fs, Us, St) ->
    Nf = length(Fs),				%# of record fields
    Nu = length(Us),				%# of update fields
    Nc = Nf - Nu,				%# of copy fields
    %% Try to be intelligent about which method of updating record to use.
    if
	Nu == 0 ->
	    {record_el(R, Name, Fs, Us),St};
	Nu == 1 ->
	    {record_setel(R, Name, Fs, Us),St};
	Nc == 1 ->
	    {record_el(R, Name, Fs, Us),St};
	true ->
	    record_match(R, Name, Fs, Us, St)
    end.	

%% record_match(Record, RecordName, [RecDefField], [Update], State)
%%  Build a 'case' expression to modify record fields.

record_match(R, Name, Fs, Us, St0) ->
    {Ps,News,St1} = record_upd_fs(Fs, Us, St0),
    Lr = element(2, hd(Us)),
    {{'case',Lr,R,
      [{clause,Lr,[{tuple,Lr,[{atom,Lr,Name}|Ps]}],[],
	[{tuple,Lr,[{atom,Lr,Name}|News]}]},
       {clause,Lr,[{var,Lr,'_'}],[],
	[call_fault(Lr, {tuple,Lr,[{atom,Lr,badrecord},{atom,Lr,Name}]})]}
      ]},
     St1}.

record_upd_fs([{record_field,Lf,{atom,La,F},Val}|Fs], Us, St0) ->
    case find_field(F, Us) of
	{ok,New} ->
	    {Ps,News,St1} = record_upd_fs(Fs, Us, St0),
	    {[{var,Lf,'_'}|Ps],[New|News],St1};
	error ->
	    {P,St1} = new_var(Lf, St0),
	    {Ps,News,St2} = record_upd_fs(Fs, Us, St1),
	    {[P|Ps],[P|News],St2}
    end;
record_upd_fs([], Us, St) -> {[],[],St}.

%% record_el(Record, RecordName, [RecDefField], [Update])
%%  Build a new record using Rec#name.field expression for unchanged
%%  values.

record_el(R, Name, Fs, []) ->
    record_el1(R, Name, Fs, [], element(2, R));
record_el(R, Name, Fs, Us) ->
    record_el1(R, Name, Fs, Us, element(2, hd(Us))).

record_el1(R, Name, Fs, Us, Lr) ->
    {tuple,Lr,[{atom,Lr,Name}|
	       map(fun (F) -> record_el2(R, Name, F, Us) end, Fs)]}.

record_el2(R, Name, {record_field,Lf,{atom,La,F},Val}, Us) ->
    case find_field(F, Us) of
	{ok,New} -> New;
	error -> {record_field,Lf,R,Name,{atom,La,F}}
    end.

%% record_setel(Record, RecordName, [RecDefField], [Update])
%%  Build a nested chain of setelement calls to build updated record tuple.

record_setel(R, Name, Fs, Us) ->
    foldr(fun ({record_field,Lf,Field,Val}, Acc) ->
		  I = index_expr(Lf, Field, Name, Fs),
		  {call,Lf,{atom,Lf,setelement},[I,Acc,Val]} end,
	  R, Us).

%% new_var_name(State) -> {VarName,State}.

new_var_name(St) ->
    C = St#expand.vcount,
    {list_to_atom("%" ++ integer_to_list(C)),St#expand{vcount=C+1}}.

%% new_var(Line, State) -> {Var,State}.

new_var(L, St0) ->
    {New,St1} = new_var_name(St0),
    {{var,L,New},St1}.

%% new_vars(Count, Line, State) -> {[Var],State}.
%%  Make Count new variables.

new_vars(N, L, St) -> new_vars(N, L, St, []).

new_vars(N, L, St0, Vs) when N > 0 ->
    {V,St1} = new_var(L, St0),
    new_vars(N-1, L, St1, [V|Vs]);
new_vars(0, L, St, Vs) -> {Vs,St}.

%% make_list(TermList, Line) ->	ConsTerm.

make_list(Ts, Line) ->
    foldr(fun (H, T) -> {cons,Line,H,T} end, {nil,Line}, Ts).

%% call_fault(Line, Reason) -> Expr.
%%  Build a call to erlang:fault/1 with reason Reason.

call_fault(L, R) ->
    {call,L,{remote,L,{atom,L,erlang},{atom,L,fault}},[R]}.

%% new_in(Before, RegionList) ->
%%	{NewInAll,NewInSome}
%%  Return the variables new in all clauses and those new in some clauses.

new_in(Before, Region) ->
    InAll = intersection(Region),
    InSome = union(Region),
    NewInAll = subtract(InAll, Before),
    NewInSome = subtract(subtract(InSome, Before), NewInAll),
    {NewInAll,NewInSome}.

%% For storing the import list we use our own version of the module dict.
%% This is/was the same as the original but we must be sure of the format
%% (sorted list of pairs) so we can do ordset operations on them (see the
%% the function eof/2. We know an empty set is [].

%% import(Line, Imports, State) ->
%%	State'
%% imported(Name, Arity, State) ->
%%	{yes,Module} | no
%%  Handle import declarations and est for imported functions. No need to
%%  check when building imports as code is correct.

import(Line, {Mod,Fs}, St) ->
    Mfs = list_to_set(Fs),
    St#expand{imports=add_imports(Mod, Mfs, St#expand.imports)}.

add_imports(Mod, [F|Fs], Is) ->
    add_imports(Mod, Fs, store(F, Mod, Is));
add_imports(Mod, [], Is) ->
    Is.

imported(F, A, St) ->
    case find({F,A}, St#expand.imports) of
	{ok,Mod} -> {yes,Mod};
	error -> no
    end.

%% This is our own version of the module dict which is/was the same as
%% the original but we must be sure of the format (sorted list of pairs)
%% so we can do ordset operations on them. We know an empty set is [].

%% find(Key, Dictionary) -> {ok,Value} | error

find(Key, [{K,Value}|D]) when Key > K -> find(Key, D);
find(Key, [{K,Value}|_]) when Key == K -> {ok,Value};
find(Key, [{K,Value}|_]) when Key < K -> error;
find(Key, []) -> error.

%% store(Key, Value, Dictionary) -> Dictionary.

store(Key, New, [{K,_}=Old|Dict]) when Key > K ->
    [Old|store(Key, New, Dict)];
store(Key, New, [{K,Old}|Dict]) when Key == K ->
    [{Key,New}|Dict];
store(Key, New, [{K,_}=Old|Dict]) when Key < K ->
    [{Key,New},Old|Dict];
store(Key, New, []) -> [{Key,New}].
