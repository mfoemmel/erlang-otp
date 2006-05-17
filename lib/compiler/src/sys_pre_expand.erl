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

-import(ordsets, [from_list/1,add_element/2,union/2]).
-import(lists,   [member/2,foldl/3,foldr/3]).

-compile({nowarn_deprecated_function, {erlang,hash,2}}).

-include("../include/erl_bits.hrl").

-record(expand, {module=[],                     %Module name
                 parameters=undefined,          %Module parameters
                 package="",                    %Module package
                 exports=[],                    %Exports
                 imports=[],                    %Imports
                 mod_imports,                   %Module Imports
                 compile=[],                    %Compile flags
                 attributes=[],                 %Attributes
                 defined=[],                    %Defined functions
                 vcount=0,                      %Variable counter
                 func=[],                       %Current function
                 arity=[],                      %Arity for current function
                 fcount=0,                      %Local fun count
                 fun_index=0,                   %Global index for funs
                 bitdefault,
                 bittypes
                }).

%% module(Forms, CompileOptions)
%%      {ModuleName,Exports,TransformedForms,CompileOptions'}
%%  Expand the forms in one module. N.B.: the lists of predefined
%%  exports and imports are really ordsets!
%%  CompileOptions is augmented with options from -compile attributes.

module(Fs0, Opts0) ->

    %% Expand records. Normalise guard tests.
    Fs = erl_expand_records:module(Fs0, Opts0),

    Opts = compiler_options(Fs) ++ Opts0,

    %% Set pre-defined exported functions.
    PreExp = [{module_info,0},{module_info,1}],

    %% Set pre-defined module imports.
    PreModImp = [{erlang,erlang},{packages,packages}],

    %% Build initial expand record.
    St0 = #expand{exports=PreExp,
                  mod_imports=dict:from_list(PreModImp),
                  compile=Opts,
                  defined=PreExp,
                  bitdefault = erl_bits:system_bitdefault(),
                  bittypes = erl_bits:system_bittypes()
                 },
    %% Expand the functions.
    {Tfs,St1} = forms(Fs, define_functions(Fs, St0)),
    {Efs,St2} = expand_pmod(Tfs, St1),
    %% Get the correct list of exported functions.
    Exports = case member(export_all, St2#expand.compile) of
                  true -> St2#expand.defined;
                  false -> St2#expand.exports
              end,
    %% Generate all functions from stored info.
    {Ats,St3} = module_attrs(St2#expand{exports = Exports}),
    {Mfs,St4} = module_predef_funcs(St3),
    {St4#expand.module, St4#expand.exports, Ats ++ Efs ++ Mfs,
     St4#expand.compile}.

compiler_options(Forms) ->
    lists:flatten([C || {attribute,_,compile,C} <- Forms]).
    
expand_pmod(Fs0, St) ->
    case St#expand.parameters of
        undefined ->
            {Fs0,St};
        Ps ->
            {Fs1,Xs,Ds} = sys_expand_pmod:forms(Fs0, Ps,
                                                St#expand.exports,
                                                St#expand.defined),
            A = length(Ps),
            Vs = [{var,0,V} || V <- Ps],
            N = {atom,0,St#expand.module},
            B = [{tuple,0,[N|Vs]}],
            F = {function,0,new,A,[{clause,0,Vs,[],B}]},
            As = St#expand.attributes,
            {[F|Fs1],St#expand{exports=add_element({new,A}, Xs),
                               defined=add_element({new,A}, Ds),
                               attributes = [{abstract, true} | As]}}
    end.

%% -type define_function(Form, State) -> State.
%%  Add function to defined if form is a function.

define_functions(Forms, #expand{defined=Predef}=St) ->
    Fs = foldl(fun({function,_,N,A,_Cs}, Acc) -> [{N,A}|Acc];
                  (_, Acc) -> Acc
               end, Predef, Forms),
    St#expand{defined=ordsets:from_list(Fs)}.

module_attrs(St) ->
    {[{attribute,0,Name,Val} || {Name,Val} <- St#expand.attributes],St}.

module_predef_funcs(St) ->
    PreDef = [{module_info,0},{module_info,1}],
    PreExp = PreDef,
    {[{function,0,module_info,0,
       [{clause,0,[],[],
        [{call,0,{remote,0,{atom,0,erlang},{atom,0,get_module_info}},
          [{atom,0,St#expand.module}]}]}]},
      {function,0,module_info,1,
       [{clause,0,[{var,0,'X'}],[],
        [{call,0,{remote,0,{atom,0,erlang},{atom,0,get_module_info}},
          [{atom,0,St#expand.module},{var,0,'X'}]}]}]}],
     St#expand{defined=union(from_list(PreDef), St#expand.defined),
               exports=union(from_list(PreExp), St#expand.exports)}}.

%% forms(Forms, State) ->
%%      {TransformedForms,State'}
%%  Process the forms. Attributes are lost and just affect the state.
%%  Ignore uninteresting forms like eof and type.

forms([{attribute,_,file,_File}=F|Fs0], St0) ->
    {Fs,St1} = forms(Fs0, St0),
    {[F|Fs],St1};
forms([{attribute,_,Name,Val}|Fs0], St0) ->
    St1 = attribute(Name, Val, St0),
    forms(Fs0, St1);
forms([{function,L,N,A,Cs}|Fs0], St0) ->
    {Ff,St1} = function(L, N, A, Cs, St0),
    {Fs,St2} = forms(Fs0, St1),
    {[Ff|Fs],St2};
forms([_|Fs], St) -> forms(Fs, St);
forms([], St) -> {[],St}.

%% -type attribute(Attribute, Value, State) ->
%%      State.
%%  Process an attribute, this just affects the state.

attribute(module, {Module, As}, St) ->
    M = package_to_string(Module),
    St#expand{module=list_to_atom(M),
              package = packages:strip_last(M),
              parameters=As};
attribute(module, Module, St) ->
    M = package_to_string(Module),
    St#expand{module=list_to_atom(M),
              package = packages:strip_last(M)};
attribute(export, Es, St) ->
    St#expand{exports=union(from_list(Es), St#expand.exports)};
attribute(import, Is, St) ->
    import(Is, St);
attribute(compile, C, St) when list(C) ->
    St#expand{compile=St#expand.compile ++ C};
attribute(compile, C, St) ->
    St#expand{compile=St#expand.compile ++ [C]};
attribute(Name, Val, St) when list(Val) ->
    St#expand{attributes=St#expand.attributes ++ [{Name,Val}]};
attribute(Name, Val, St) ->
    St#expand{attributes=St#expand.attributes ++ [{Name,[Val]}]}.

function(L, N, A, Cs0, St0) ->
    {Cs,St} = clauses(Cs0, St0#expand{func=N,arity=A,fcount=0}),
    {{function,L,N,A,Cs},St}.

%% -type clauses([Clause], State) ->
%%      {[TransformedClause],State}.
%%  Expand function clauses.

clauses([{clause,Line,H0,G0,B0}|Cs0], St0) ->
    {H,St1} = head(H0, St0),
    {G,St2} = guard(G0, St1),
    {B,St3} = exprs(B0, St2),
    {Cs,St4} = clauses(Cs0, St3),
    {[{clause,Line,H,G,B}|Cs],St4};
clauses([], St) -> {[],St}.

%% head(HeadPatterns, State) ->
%%      {TransformedPatterns,Variables,UsedVariables,State'}

head(As, St) -> pattern_list(As, St).

%% pattern(Pattern, State) ->
%%      {TransformedPattern,State'}
%%

pattern({var,_,'_'}=Var, St) ->                 %Ignore anonymous variable.
    {Var,St};
pattern({var,_,_}=Var, St) ->
    {Var,St};
pattern({char,_,_}=Char, St) ->
    {Char,St};
pattern({integer,_,_}=Int, St) ->
    {Int,St};
pattern({float,_,_}=Float, St) ->
    {Float,St};
pattern({atom,_,_}=Atom, St) ->
    {Atom,St};
pattern({string,_,_}=String, St) ->
    {String,St};
pattern({nil,_}=Nil, St) ->
    {Nil,St};
pattern({cons,Line,H,T}, St0) ->
    {TH,St1} = pattern(H, St0),
    {TT,St2} = pattern(T, St1),
    {{cons,Line,TH,TT},St2};
pattern({tuple,Line,Ps}, St0) ->
    {TPs,St1} = pattern_list(Ps, St0),
    {{tuple,Line,TPs},St1};
%%pattern({struct,Line,Tag,Ps}, St0) ->
%%    {TPs,TPsvs,St1} = pattern_list(Ps, St0),
%%    {{tuple,Line,[{atom,Line,Tag}|TPs]},TPsvs,St1};
pattern({record_field,_,_,_}=M, St) ->
    {expand_package(M, St),St};  % must be a package name
pattern({bin,Line,Es0}, St0) ->
    {Es1,St1} = pattern_bin(Es0, St0),
    {{bin,Line,Es1},St1};
pattern({op,_,'++',{nil,_},R}, St) ->
    pattern(R, St);
pattern({op,_,'++',{cons,Li,H,T},R}, St) ->
    pattern({cons,Li,H,{op,Li,'++',T,R}}, St);
pattern({op,_,'++',{string,Li,L},R}, St) ->
    pattern(string_to_conses(Li, L, R), St);
pattern({match,Line,Pat1, Pat2}, St0) ->
    {TH,St1} = pattern(Pat2, St0),
    {TT,St2} = pattern(Pat1, St1),
    {{match,Line,TT,TH},St2};
%% Compile-time pattern expressions, including unary operators.
pattern({op,Line,Op,A}, St) ->
    {erl_eval:partial_eval({op,Line,Op,A}),St};
pattern({op,Line,Op,L,R}, St) ->
    {erl_eval:partial_eval({op,Line,Op,L,R}),St}.

pattern_list([P0|Ps0], St0) ->
    {P,St1} = pattern(P0, St0),
    {Ps,St2} = pattern_list(Ps0, St1),
    {[P|Ps],St2};
pattern_list([], St) -> {[],St}.

%% guard(Guard, State) ->
%%      {TransformedGuard,State'}
%%  Transform a list of guard tests. We KNOW that this has been checked
%%  and what the guards test are. Use expr for transforming the guard
%%  expressions.

guard([G0|Gs0], St0) ->
    {G,St1} = guard_tests(G0, St0),
    {Gs,St2} = guard(Gs0, St1),
    {[G|Gs],St2};
guard([], St) -> {[],St}.

guard_tests([Gt0|Gts0], St0) ->
    {Gt1,St1} = guard_test(Gt0, St0),
    {Gts1,St2} = guard_tests(Gts0, St1),
    {[Gt1|Gts1],St2};
guard_tests([], St) -> {[],St}.

%% The following clause as well as normalise_test/1 can be removed 
%% as soon as R11B has been released.
guard_test({call,Line,{atom,Lt,Tname},As}, St) ->
    Test = {remote,Lt,
            {atom,Lt,erlang},
            {atom,Lt,normalise_test(Tname, length(As))}},
    expr({call,Line,Test,As}, St);
guard_test(Test, St) ->
    expr(Test, St).

normalise_test(atom, 1)      -> is_atom;
normalise_test(binary, 1)    -> is_binary;
normalise_test(constant, 1)  -> is_constant;
normalise_test(float, 1)     -> is_float;
normalise_test(function, 1)  -> is_function;
normalise_test(integer, 1)   -> is_integer;
normalise_test(list, 1)      -> is_list;
normalise_test(number, 1)    -> is_number;
normalise_test(pid, 1)       -> is_pid; 
normalise_test(port, 1)      -> is_port; 
normalise_test(reference, 1) -> is_reference;
normalise_test(tuple, 1)     -> is_tuple;
normalise_test(Name, _) -> Name.

%% exprs(Expressions, State) ->
%%      {TransformedExprs,State'}

exprs([E0|Es0], St0) ->
    {E,St1} = expr(E0, St0),
    {Es,St2} = exprs(Es0, St1),
    {[E|Es],St2};
exprs([], St) -> {[],St}.

%% expr(Expression, State) ->
%%      {TransformedExpression,State'}

expr({var,_,_}=Var, St) ->
    {Var,St};
expr({char,_,_}=Char, St) ->
    {Char,St};
expr({integer,_,_}=Int, St) ->
    {Int,St};
expr({float,_,_}=Float, St) ->
    {Float,St};
expr({atom,_,_}=Atom, St) ->
    {Atom,St};
expr({string,_,_}=String, St) ->
    {String,St};
expr({nil,_}=Nil, St) ->
    {Nil,St};
expr({cons,Line,H0,T0}, St0) ->
    {H,St1} = expr(H0, St0),
    {T,St2} = expr(T0, St1),
    {{cons,Line,H,T},St2};
expr({lc,Line,E0,Qs0}, St0) ->
    {Qs1,St1} = lc_tq(Line, Qs0, St0),
    {E1,St2} = expr(E0, St1),
    {{lc,Line,E1,Qs1},St2};
expr({tuple,Line,Es0}, St0) ->
    {Es1,St1} = expr_list(Es0, St0),
    {{tuple,Line,Es1},St1};
%%expr({struct,Line,Tag,Es0}, Vs, St0) ->
%%    {Es1,Esvs,Esus,St1} = expr_list(Es0, Vs, St0),
%%    {{tuple,Line,[{atom,Line,Tag}|Es1]},Esvs,Esus,St1};
expr({record_field,_,_,_}=M, St) ->
    {expand_package(M, St),St};  % must be a package name
expr({bin,Line,Es0}, St0) ->
    {Es1,St1} = expr_bin(Es0, St0),
    {{bin,Line,Es1},St1};
expr({block,Line,Es0}, St0) ->
    {Es,St1} = exprs(Es0, St0),
    {{block,Line,Es},St1};
expr({'if',Line,Cs0}, St0) ->
    {Cs,St1} = icr_clauses(Cs0, St0),
    {{'if',Line,Cs},St1};
expr({'case',Line,E0,Cs0}, St0) ->
    {E,St1} = expr(E0, St0),
    {Cs,St2} = icr_clauses(Cs0, St1),
    {{'case',Line,E,Cs},St2};
expr({'receive',Line,Cs0}, St0) ->
    {Cs,St1} = icr_clauses(Cs0, St0),
    {{'receive',Line,Cs},St1};
expr({'receive',Line,Cs0,To0,ToEs0}, St0) ->
    {To,St1} = expr(To0, St0),
    {ToEs,St2} = exprs(ToEs0, St1),
    {Cs,St3} = icr_clauses(Cs0, St2),
    {{'receive',Line,Cs,To,ToEs},St3};
expr({'fun',Line,Body}, St) ->
    fun_tq(Line, Body, St);
expr({'cond',Line,Cs}, St0) ->
    {V,St1} = new_var(Line,St0),
    expr(cond_clauses(Cs, V), St1);
expr({call,Line,{atom,La,N},As0}, St0) ->
    {As,St1} = expr_list(As0, St0),
    Ar = length(As),
    case erl_internal:bif(N, Ar) of
        true ->
            {{call,Line,{remote,La,{atom,La,erlang},{atom,La,N}},As},St1};
        false ->
            case imported(N, Ar, St1) of
                {yes,Mod} ->
                    {{call,Line,{remote,La,{atom,La,Mod},{atom,La,N}},As},St1};
                no ->
                    {{call,Line,{atom,La,N},As},St1}
            end
    end;
expr({call,Line,{record_field,_,_,_}=M,As0}, St0) ->
    expr({call,Line,expand_package(M, St0),As0}, St0);
expr({call,Line,{remote,Lr,M,F},As0}, St0) ->
    M1 = expand_package(M, St0),
    {[M2,F1|As1],St1} = expr_list([M1,F|As0], St0),
    {{call,Line,{remote,Lr,M2,F1},As1},St1};
expr({call,Line,{tuple,_,[{atom,_,_}=M,{atom,_,_}=F]},As}, St) ->
    %% Rewrite {Mod,Function}(Args...) to Mod:Function(Args...).
    expr({call,Line,{remote,Line,M,F},As}, St);
expr({call,Line,F,As0}, St0) ->
    {[Fun1|As1],St1} = expr_list([F|As0], St0),
    {{call,Line,Fun1,As1},St1};
expr({'try',Line,Es0,Scs0,Ccs0,As0}, St0) ->
    {Es1,St1} = exprs(Es0, St0),
    {Scs1,St2} = icr_clauses(Scs0, St1),
    {Ccs1,St3} = icr_clauses(Ccs0, St2),
    {As1,St4} = exprs(As0, St3),
    {{'try',Line,Es1,Scs1,Ccs1,As1},St4};
expr({'catch',Line,E0}, St0) ->
    %% Catch exports no new variables.
    {E,St1} = expr(E0, St0),
    {{'catch',Line,E},St1};
expr({match,Line,P0,E0}, St0) ->
    {E,St1} = expr(E0, St0),
    {P,St2} = pattern(P0, St1),
    {{match,Line,P,E},St2};
expr({op,_,'++',{string,L1,S1},{string,_,S2}}, St) ->
    {{string,L1,S1 ++ S2},St};
expr({op,Ll,'++',{string,L1,S1}=Str,R0}, St0) ->
    {R1,St1} = expr(R0, St0),
    E = case R1 of
            {string,_,S2} -> {string,L1,S1 ++ S2};
            _Other when length(S1) < 8 -> string_to_conses(L1, S1, R1);
            _Other -> {op,Ll,'++',Str,R1}
        end,
    {E,St1};
expr({op,Ll,'++',{cons,Lc,H,T},L2}, St) ->
    expr({cons,Ll,H,{op,Lc,'++',T,L2}}, St);
expr({op,_,'++',{nil,_},L2}, St) ->
    expr(L2, St);
expr({op,Line,Op,A0}, St0) ->
    {A,St1} = expr(A0, St0),
    {{op,Line,Op,A},St1};
expr({op,Line,Op,L0,R0}, St0) ->
    {L,St1} = expr(L0, St0),
    {R,St2} = expr(R0, St1),
    {{op,Line,Op,L,R},St2}.

expr_list([E0|Es0], St0) ->
    {E,St1} = expr(E0, St0),
    {Es,St2} = expr_list(Es0, St1),
    {[E|Es],St2};
expr_list([], St) -> {[],St}.

%% icr_clauses([Clause], State) -> {[TransformedClause],State'}
%%  Be very careful here to return the variables that are really used
%%  and really new.

icr_clauses([], St) -> {[],St};
icr_clauses(Clauses, St) -> icr_clauses2(Clauses, St).

icr_clauses2([{clause,Line,H0,G0,B0}|Cs0], St0) ->
    {H,St1} = head(H0, St0),
    {G,St2} = guard(G0, St1),
    {B,St3} = exprs(B0, St2),
    {Cs,St4} = icr_clauses2(Cs0, St3),
    {[{clause,Line,H,G,B}|Cs],St4};
icr_clauses2([], St) -> {[],St}.

%% lc_tq(Line, Qualifiers, State) ->
%%      {[TransQual],State'}

lc_tq(Line, [{generate,Lg,P0,G0} | Qs0], St0) ->
    {G1,St1} = expr(G0, St0),
    {P1,St2} = pattern(P0, St1),
    {Qs1,St3} = lc_tq(Line, Qs0, St2),
    {[{generate,Lg,P1,G1} | Qs1],St3};
lc_tq(Line, [F0 | Qs0], St0) ->
    case erl_lint:is_guard_test(F0) of
        true ->
            {F1,St1} = guard_test(F0, St0),
            {Qs1,St2} = lc_tq(Line, Qs0, St1),
            {[F1|Qs1],St2};
        false ->
            {F1,St1} = expr(F0, St0),
            {Qs1,St2} = lc_tq(Line, Qs0, St1),
            {[F1 | Qs1],St2}
    end;
lc_tq(_Line, [], St0) ->
    {[],St0}.

%% fun_tq(Line, Body, State) ->
%%      {Fun,State'}
%% Transform an "explicit" fun {'fun', Line, {clauses, Cs}} into an
%% extended form {'fun', Line, {clauses, Cs}, Info}, unless it is the
%% name of a BIF (erl_lint has checked that it is not an import).
%% Process the body sequence directly to get the new and used variables.
%% "Implicit" funs {'fun', Line, {function, F, A}} are not changed.

fun_tq(Lf, {function,F,A}, St0) ->
    {As,St1} = new_vars(A, Lf, St0),
    Cs = [{clause,Lf,As,[],[{call,Lf,{atom,Lf,F},As}]}],
    case erl_internal:bif(F, A) of
        true ->
            fun_tq(Lf, {clauses,Cs}, St1);
        false ->
            Index = St0#expand.fun_index,
            Uniq = erlang:hash(Cs, (1 bsl 27)-1),
            {Fname,St2} = new_fun_name(St1),
            {{'fun',Lf,{function,F,A},{Index,Uniq,Fname}},
             St2#expand{fun_index=Index+1}}
    end;
fun_tq(L, {function,M,F,A}, St) ->
    {{call,L,{remote,L,{atom,L,erlang},{atom,L,make_fun}},
      [{atom,L,M},{atom,L,F},{integer,L,A}]},St};
fun_tq(Lf, {clauses,Cs0}, St0) ->
    Uniq = erlang:hash(Cs0, (1 bsl 27)-1),
    {Cs1,St1} = fun_clauses(Cs0, St0),
    Index = St1#expand.fun_index,
    {Fname,St2} = new_fun_name(St1),
    {{'fun',Lf,{clauses,Cs1},{Index,Uniq,Fname}},
     St2#expand{fun_index=Index+1}}.

fun_clauses([{clause,L,H0,G0,B0}|Cs0], St0) ->
    {H,St1} = head(H0, St0),
    {G,St2} = guard(G0, St1),
    {B,St3} = exprs(B0, St2),
    {Cs,St4} = fun_clauses(Cs0, St3),
    {[{clause,L,H,G,B}|Cs],St4};
fun_clauses([], St) -> {[],St}.

%% new_fun_name(State) -> {FunName,State}.

new_fun_name(#expand{func=F,arity=A,fcount=I}=St) ->
    Name = "-" ++ atom_to_list(F) ++ "/" ++ integer_to_list(A)
        ++ "-fun-" ++ integer_to_list(I) ++ "-",
    {list_to_atom(Name),St#expand{fcount=I+1}}.

%% pattern_bin([Element], State) -> {[Element],[Variable],[UsedVar],State}.

pattern_bin(Es0, St) ->
    Es1 = bin_expand_strings(Es0),
    foldr(fun (E, Acc) -> pattern_element(E, Acc) end, {[],St}, Es1).

pattern_element({bin_element,Line,Expr0,Size0,Type0}, {Es,St0}) ->
    {Expr1,St1} = pattern(Expr0, St0),
    {Size1,St2} = pat_bit_size(Size0, St1),
    {Size,Type} = make_bit_type(Line, Size1, Type0),
    Expr = coerce_to_float(Expr1, Type0),
    {[{bin_element,Line,Expr,Size,Type}|Es],St2}.

pat_bit_size(default, St) -> {default,St};
pat_bit_size({atom,_La,all}=All, St) -> {All,St};
pat_bit_size({var,_Lv,_V}=Var, St) -> {Var,St};
pat_bit_size(Size, St) ->
    Line = element(2, Size),
    {value,Sz,_} = erl_eval:expr(Size, erl_eval:new_bindings()),
    {{integer,Line,Sz},St}.

make_bit_type(Line, default, Type0) ->
    case erl_bits:set_bit_type(default, Type0) of
        {ok,all,Bt} -> {{atom,Line,all},erl_bits:as_list(Bt)};
        {ok,Size,Bt} -> {{integer,Line,Size},erl_bits:as_list(Bt)}
    end;
make_bit_type(_Line, Size, Type0) ->            %Integer or 'all'
    {ok,Size,Bt} = erl_bits:set_bit_type(Size, Type0),
    {Size,erl_bits:as_list(Bt)}.

coerce_to_float({integer,L,I}=E, [float|_]) ->
    try
        {float,L,float(I)}
    catch
        error:badarg -> E;
        error:badarith -> E
    end;
coerce_to_float(E, _) -> E.
    
%% expr_bin([Element], State) -> {[Element],State}.

expr_bin(Es0, St) ->
    Es1 = bin_expand_strings(Es0),
    foldr(fun (E, Acc) -> bin_element(E, Acc) end, {[],St}, Es1).

bin_element({bin_element,Line,Expr,Size,Type}, {Es,St0}) ->
    {Expr1,St1} = expr(Expr, St0),
    {Size1,St2} = if Size == default -> {default,St1};
                             true -> expr(Size, St1)
                          end,
    {Size2,Type1} = make_bit_type(Line, Size1, Type),
    {[{bin_element,Line,Expr1,Size2,Type1}|Es],St2}.

bin_expand_strings(Es) ->
    foldr(fun ({bin_element,Line,{string,_,S},default,default}, Es1) ->
                  foldr(fun (C, Es2) ->
                                [{bin_element,Line,{char,Line,C},default,default}|Es2]
                        end, Es1, S);
              (E, Es1) -> [E|Es1]
          end, [], Es).

%% new_var_name(State) -> {VarName,State}.

new_var_name(St) ->
    C = St#expand.vcount,
    {list_to_atom("pre" ++ integer_to_list(C)),St#expand{vcount=C+1}}.

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
new_vars(0, _L, St, Vs) -> {Vs,St}.

string_to_conses(Line, Cs, Tail) ->
    foldr(fun (C, T) -> {cons,Line,{char,Line,C},T} end, Tail, Cs).


%% In syntax trees, module/package names are atoms or lists of atoms.

package_to_string(A) when atom(A) -> atom_to_list(A);
package_to_string(L) when list(L) -> packages:concat(L).

expand_package({atom,L,A} = M, St) ->
    case dict:find(A, St#expand.mod_imports) of
        {ok, A1} ->
            {atom,L,A1};
        error ->
            case packages:is_segmented(A) of
                true ->
                    M;
                false -> 
                    M1 = packages:concat(St#expand.package, A),
                    {atom,L,list_to_atom(M1)}
            end
    end;
expand_package(M, _St) ->
    case erl_parse:package_segments(M) of
        error ->
            M;
        M1 ->
            {atom,element(2,M),list_to_atom(package_to_string(M1))}
    end.

%% Create a case-switch on true/false, generating badarg for all other
%% values.

make_bool_switch(L, E, V, T, F) ->
    NegL = -abs(L),
    {'case',NegL,E,
     [{clause,NegL,[{atom,NegL,true}],[],T},
      {clause,NegL,[{atom,NegL,false}],[],F},
      {clause,NegL,[V],[],
       [call_error(NegL,{tuple,NegL,[{atom,NegL,badarg},V]})]}]}.

%% Expand a list of cond-clauses to a sequence of case-switches.

cond_clauses([{clause,L,[],[[E]],B}],V) ->
    make_bool_switch(L,E,V,B,[call_error(L,{atom,L,cond_clause})]);
cond_clauses([{clause,L,[],[[E]],B} | Cs],V) ->
    make_bool_switch(L,E,V,B,[cond_clauses(Cs,V)]).

%% call_error(Line, Reason) -> Expr.
%%  Build a call to erlang:error/1 with reason Reason.

call_error(L, R) ->
    {call,L,{remote,L,{atom,L,erlang},{atom,L,error}},[R]}.

%% import(Line, Imports, State) ->
%%      State'
%% imported(Name, Arity, State) ->
%%      {yes,Module} | no
%%  Handle import declarations and est for imported functions. No need to
%%  check when building imports as code is correct.

import({Mod0,Fs}, St) ->
    Mod = list_to_atom(package_to_string(Mod0)),
    Mfs = from_list(Fs),
    St#expand{imports=add_imports(Mod, Mfs, St#expand.imports)};
import(Mod0, St) ->
    Mod = package_to_string(Mod0),
    Key = list_to_atom(packages:last(Mod)),
    St#expand{mod_imports=dict:store(Key, list_to_atom(Mod),
                                     St#expand.mod_imports)}.

add_imports(Mod, [F|Fs], Is) ->
    add_imports(Mod, Fs, orddict:store(F, Mod, Is));
add_imports(_, [], Is) -> Is.

imported(F, A, St) ->
    case orddict:find({F,A}, St#expand.imports) of
        {ok,Mod} -> {yes,Mod};
        error -> no
    end.
