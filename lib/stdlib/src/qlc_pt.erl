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
-module(qlc_pt).

%%% Purpose: Implements the QLC Parse Transform.

-export([parse_transform/2, transform_from_evaluator/2, 
         transform_expression/2]).

%% Exported to qlc.erl only:
-export([vars/1, aux_name1/3]).

-import(lists, [foldl/3, map/2, sort/1]).

-include_lib("stdlib/include/ms_transform.hrl").

-define(APIMOD, qlc).
-define(Q, q).

%% Also in qlc.erl.
-define(QLC_Q(L1, L2, L3, L4, LC, Os), 
        {call,L1,{remote,L2,{atom,L3,?APIMOD},{atom,L4,?Q}},[LC | Os]}).
-define(IMP_Q(L1, L2, LC, Os), {call,L,{atom,L2,?Q},[LC | Os]}).

%% Also in qlc.erl.
-record(qlc_lc,     % qlc:q/1,2, a query handle
        {lc,
         opt        % #qlc_opt
        }).

-record(state, {imp, maxargs, records}).

%-define(debug, true).

-ifdef(debug).
-define(DEBUG(S, A), io:format(S, A)).
-else.
-define(DEBUG(S, A), ok).
-endif.

%% erl_eval cannot interpret funs with more than 20 arguments:
-define(EVAL_MAX_NUM_OF_ARGS, 20).
%% Currently the compiler can handle at most 255 arguments.
-define(COMPILE_MAX_NUM_OF_ARGS, 250).

%%%
%%% Exported functions
%%%

parse_transform(Forms0, Options) ->
    Forms = Forms0,
    ?DEBUG("QLC Parse Transform~n", []),
    State = #state{imp = is_qlc_q_imported(Forms),
                   maxargs = ?COMPILE_MAX_NUM_OF_ARGS,
                   records = record_attributes(Forms)},
    FormsNoShadows = no_shadows(Forms, State),
    case compile_messages(Forms, FormsNoShadows, Options, State) of
        {[],[],Warnings} ->
            NewForms = transform(FormsNoShadows, State),
            {[],WForms} = no_duplicates(NewForms, [], Warnings, Options),
            NewForms ++ WForms;
        {E0,Errors,Warnings} ->
            {EForms,WForms} = no_duplicates(Forms, Errors, Warnings, Options),
            sort(E0 ++ EForms) ++ Forms ++ WForms
    end.

transform_from_evaluator(LC, Bindings) ->
    ?DEBUG("QLC Parse Transform (Evaluator Version)~n", []),
    transform_expression(LC, Bindings, false).

transform_expression(LC, Bindings) ->
    transform_expression(LC, Bindings, true).

%%%
%%% Local functions
%%%

transform_expression(LC, Bs0, WithLintErrors) ->
    L = 1,
    As = map(fun({V,_Val}) -> {var,L,V} end, Bs0),
    F = {function,L,bar,L,[{clause,L,As,[],[?QLC_Q(L, L, L, L, LC, [])]}]},
    Forms = [{attribute,L,file,{"foo",L}},
             {attribute,L,module,foo}, F],
    State = #state{imp = false,
                   maxargs = ?EVAL_MAX_NUM_OF_ARGS,
                   records = record_attributes(Forms)},
    Options = [],
    FormsNoShadows = no_shadows(Forms, State),
    case compile_messages(Forms, FormsNoShadows, Options, State) of
        {[],[],_Warnings} ->
            NewForms = transform(FormsNoShadows, State),
            {function,L,bar,L,[{clause,L,As,[],[NF]}]} = lists:last(NewForms),
            {ok,NF};
        {E0,Errors,_Warnings} when WithLintErrors =:= true ->
            {not_ok,sort(E0 ++ mforms(error, Errors))};
        {E0,Errors,Warnings} ->
            {EForms,_} = no_duplicates(Forms, Errors, Warnings, Options),
            [{error, Reason} | _] = sort(E0 ++ EForms),
            {not_ok, {error, ?APIMOD, Reason}}
    end.

-define(I(I), {integer, L, I}).
-define(A(A), {atom, L, A}).
-define(V(V), {var, L, V}).
-define(ABST_NO_MORE, {nil, L}).
-define(ABST_MORE(Obj, Cont), {cons, L, Obj, Cont}).

%% Qualifier identifier. 
%% The first one encountered in a QLC expression has no=1.
-record(qid, {lcid,no}).

%% Avoid duplicated lint warnings and lint errors. Care has been taken
%% not to introduce unused variables in the transformed code.
%%
no_duplicates(Forms, Errors, Warnings, Options) ->
    {Es1,Ws1} = compile_forms(Forms, Options),
    Es = mforms(error, Errors) -- mforms(error, Es1),
    Ws = mforms(warning, Warnings) -- mforms(warning, Ws1),
    {Es,Ws}.

mforms(Tag, L) ->
    [{Tag,M} || {_File,Ms} <- L, M <- Ms].

is_qlc_q_imported(Forms) ->
    [[] || {attribute,_,import,{?APIMOD,FAs}} <- Forms, {?Q,1} <- FAs] =/= [].

record_attributes(Forms) ->
    [A || A = {attribute, _, record, _D} <- Forms].

%% Get the compile errors and warnings for the QLC expressions as well
%% as messages for introduced variables used in list expressions and
%% messages for badargs. Since the QLC expressions will be replaced by
%% some terms, the compiler cannot find the errors and warnings after
%% the parse transformation.
%%
compile_messages(Forms, FormsNoShadows, Options, State) ->
    GenForm = used_genvar_check(FormsNoShadows, State),
    ?DEBUG("GenForm = ~s~n", [catch erl_pp:form(GenForm)]),
    WarnFun = fun(Id, LC, A) -> {tag_lines(LC, get_lcid_no(Id)), A} end,
    {WForms,ok} = qlc_mapfold(WarnFun, ok, Forms, State),
    {Es, Ws} =
        case compile_forms(WForms ++ [GenForm], Options) of
            {[], Warnings} ->
                ?DEBUG("Warnings = ~p~n", [Warnings]),
                {[],tagged_messages(Warnings)};
            {Errors, Warnings} ->
                {tagged_messages(Errors),tagged_messages(Warnings)}
    end,
    {badarg(Forms, State),Es,Ws}.

badarg(Forms, State) ->
    F = fun(_Id, {lc,_L,_E,_Qs}=LC, Es) -> 
                {LC,Es};
           (Id, A, Es) -> 
                E = {get_lcid_line(Id),?APIMOD,not_a_query_list_comprehension},
                {A,[{error,E} | Es]}
        end,
    {_,E0} = qlc_mapfold(F, [], Forms, State),
    E0.

%% Adopted from erl_lint:copy_expr/2.

tag_lines(E, No) ->
    map_lines(fun(Id) -> 
                      case is_lcid(Id) of
                          true -> Id;
                          false -> make_lcid(Id, No)
                      end
              end, E).

map_lines(F, {clauses,Cs}) -> {clauses,map_lines(F, Cs)};
map_lines(_F, {function,Fun,A}) -> {function,Fun,A};
map_lines(F, {Tag,L}) -> {Tag,F(L)};
map_lines(F, {Tag,L,E1}) ->
    {Tag,F(L),map_lines(F, E1)};
map_lines(F, {Tag,L,E1,E2}) ->
    {Tag,F(L),map_lines(F, E1),map_lines(F, E2)};
map_lines(F, {bin_element,L,E1,E2,TSL}) ->
    {bin_element,F(L),map_lines(F, E1), map_lines(F, E2), TSL};
map_lines(F, {Tag,L,E1,E2,E3}) ->
    {Tag,F(L),map_lines(F, E1),map_lines(F, E2),map_lines(F, E3)};
map_lines(F, {Tag,L,E1,E2,E3,E4}) -> % not used
    {Tag,F(L),map_lines(F, E1), map_lines(F, E2),
     map_lines(F, E3), map_lines(F, E4)};
map_lines(F, [H | T]) ->
    [map_lines(F, H) | map_lines(F, T)];
map_lines(_F, []) -> [];
map_lines(_F, E) when is_constant(E) -> E.

tagged_messages(MsL) ->
    [{File,
      [{get_lcid_line(Id),Mod,untag(T)} || {Id,Mod,T} <- Ms,
                                           is_lcid(Id)]}
     || {File,Ms} <- MsL]
    ++
    [{File,
      [{Line,?APIMOD,{used_generator_variable,V}} 
           || {{extra,Line,V},erl_lint,{unbound_var,_}} <- Ms]}
     || {File,Ms} <- MsL].

untag([E | Es]) -> [untag(E) | untag(Es)];
untag(T) when is_tuple(T) -> list_to_tuple(untag(tuple_to_list(T)));
untag(E) ->
    case is_lcid(E) of
        true -> get_lcid_line(E);
        false -> E
    end.

%% -> [{Qid,[variable()]}].
%%
%% For each qualifier the introduced variables are found. The
%% variables introduced in filters are very much like the variables
%% introduced in generator patterns. If no variables are introduced in
%% a qualifier, [variable()] is empty.
%%
%% Generator: all variables occurring in the pattern are introduced
%% variables.
%% Filter: all variables bound inside the filter are introduced
%% variables (unless they are unsafe).
%%
intro_variables(Forms, State) ->
    Fun = fun(QId, {generate,_L,P0,_E0}=Q, {GVs,QIds}, Foo) ->
                  PVs = var_ufold(fun({var,_,V}) -> {QId,V} end, P0),
                  {Q,{ordsets:to_list(PVs) ++ GVs,[{QId,[]} | QIds]},Foo};
             (QId, Filter0, {GVs,QIds}, Foo) ->
                  %% The filter F is replaced by begin E, F, E end,
                  %% where E is an LC expression consisting of a
                  %% template mentioning all variables occurring in F.
                  Vs = ordsets:to_list(vars(Filter0)),
                  Id = QId#qid.lcid,
                  LC1 = embed_vars([{var,{QId,f1},V} || V <- Vs], Id),
                  LC2 = embed_vars([{var,{QId,f2},V} || V <- Vs], Id),
                  Filter = {block,line,[LC1,Filter0,LC2]},
                  {Filter,{GVs,[{QId,[]} | QIds]},Foo}
          end,
    Acc0 = {[],[]},
    {FForms,{GenVars,QIds}} = qual_fold(Fun, Acc0, [], Forms, State),
    Es0 = compile_errors(FForms),
    %% A variable is bound inside the filter if it is not bound before
    %% the filter, but it is bound after the filter (obviously).
    Before = [{QId,V} || {{QId,f1},erl_lint,{unbound_var,V}} <- Es0],
    After = [{QId,V} || {{QId,f2},erl_lint,{unbound_var,V}} <- Es0],
    Unsafe = [{QId,V} || {{QId,f2},erl_lint,{unsafe_var,V,_Where}} <- Es0],
    ?DEBUG("Before = ~p~n", [Before]),
    ?DEBUG("After = ~p~n", [After]),
    ?DEBUG("Unsafe = ~p~n", [Unsafe]),
    ?DEBUG("Filter ~p~n", [(Before -- After) -- Unsafe]),
    IV = (Before -- After) -- Unsafe,
    I1 = family(IV ++ GenVars),
    sofs:to_external(sofs:family_union(sofs:family(QIds), I1)).

compile_errors(Forms) ->
    case compile_forms(Forms, []) of
        {[], _Warnings} ->
            [];
        {Errors, _Warnings} ->
            ?DEBUG("got errors ~p~n", [Errors]),
            lists:flatmap(fun({_File,Es}) -> Es end, Errors)
    end.

compile_forms(Forms0, Options) ->
    Forms1 = no_qlc_transform(Forms0),
    Forms = [F || F <- Forms1, element(1, F) =/= eof] ++ [{eof,999999}],
    case compile:forms(Forms, compile_options(Options)) of
        {ok, _ModName, _Binary, Ws0} -> 
            {[], Ws0};
        {error, Es0, Ws0} -> 
            {Es0, Ws0}
    end.

no_qlc_transform([{attribute, _, compile, {parse_transform,?APIMOD}} | Fs]) ->
    no_qlc_transform(Fs);
no_qlc_transform([F | Fs]) ->
    [F | no_qlc_transform(Fs)];
no_qlc_transform([]) ->
    [].

compile_options(Options) ->
    No = [report,report_errors,report_warnings],
    [O || O <- [return | Options], not lists:member(O, No)].

%% In LCs it is possible to use variables introduced in filters and
%% generator patterns in the right hand side of generators (ListExpr),
%% but in QLCs this is not allowed. 
%%
%% A brand new function is returned such that there is one expression
%% for each ListExpr. The expression mentions all introduced variables
%% occurring in ListExpr. Running the function through the compiler
%% yields error messages for erroneous use of introduced variables.
%% The messages have the form
%% {{extra,LineNo,Var},Module,{unbound_var,V}}, where Var is the
%% original variable name and V is the name invented by no_shadows/2.
%%
used_genvar_check(Forms, State) ->
    F = fun(QId, {generate, Ln, _P, LE}=Q, {QsIVs0, Exprs0}, IVsSoFar0) ->
                F = fun({var, _, V}=Var) -> 
                            {var, L, OrigVar} = undo_no_shadows(Var),
                            {var, {extra, L, OrigVar}, V} 
                    end,
                Vs = [Var || {var, _, V}=Var <- var_fold(F, [], LE),
                             lists:member(V, IVsSoFar0)],
                Exprs = case Vs of
                            [] -> Exprs0;
                            _ -> [embed_vars(Vs, Ln) | Exprs0]
                        end,
                {QsIVs,IVsSoFar} = q_intro_vars(QId, QsIVs0, IVsSoFar0),
                {Q, {QsIVs, Exprs}, IVsSoFar};
           (QId, Filter, {QsIVs0, Exprs}, IVsSoFar0) ->
                {QsIVs, IVsSoFar} = q_intro_vars(QId, QsIVs0, IVsSoFar0),
                {Filter, {QsIVs, Exprs}, IVsSoFar}
        end,
    IntroVars = intro_variables(Forms, State),
    Acc0 = {IntroVars, [{atom, 0, true}]},
    {_, {[], Exprs}} = qual_fold(F, Acc0, [], Forms, State),
    FunctionNames = [Name || {function, _, Name, _, _} <- Forms],
    UniqueFName = aux_name(used_genvar, 1, sets:from_list(FunctionNames)),
    {function,0,UniqueFName,0,[{clause,0,[],[],lists:reverse(Exprs)}]}.
    
q_intro_vars(QId, [{QId, IVs} | QsIVs], IVsSoFar) -> {QsIVs, IVs ++ IVsSoFar}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% The transformed code has two major parts: a fun where each
%% qualifier is represented by one or more clauses, and a table where
%% list expressions (the right hand side of generators, LE) are
%% represented by funs (the table is further processed in runtime).
%% The separation into a fun and a table makes it possible to
%% rearrange qualifiers while keeping the speed offered by compiled
%% code, and to run the LEs before evaluation of the QLC (and possibly
%% modify the LEs should that be necessary). No rearrangements of the
%% elements of the table are done in this version of QLC.
%%
%% For each QLC, every filter is given a state number and every
%% generator two state numbers (one for initialization, one for
%% looping over values). State 1 is reserved for the template and
%% state 0 is entered when there are no more values to try, so
%% assuming no rearrangement of the qualifiers has taken place, the
%% first qualifier is given state number 2. For every state except 0,
%% the table tells which state to go to next. By modifying the table,
%% the order of the qualifiers can be altered at runtime.
%%
%% The syntax of the value Val returned from the fun is:
%% Val = [] | [term() | Val] | fun() -> Val
%% Note: the fun must not return a fun if it to be called by
%% the function outlined below.
%%
%% An outline of the generated fun:
%%
%% fun(0, RL, ...) when is_list(RL) -> % the final state
%%       lists:reverse(RL);   % eval, all answers collected in a list
%%    (0, ...) -> [];    % cursor (or fold)
%%    (1, RL, ...) when is_list(RL) -> % the template state
%%       Fun(<last generator loop state>, [Template | RL], ...);
%%    (1, ....) ->            % return the object and a continuation
%%       [Template | fun() -> Fun(<last generator loop state>, ...)];
%%    (2, ...) -> % an sample generator, initialization state
%%       Fun(3, ..., <initial value>, ...);
%%    (3, ..., [Pattern | Val], ...) -> % looping over values (a list)
%%       Fun(<next qualifier state>, ..., Val, ...); % arguments are bound
%%    (3, ..., [_ | Val], ...) -> % pattern does not match
%%       Fun(3, ..., Val, ...);
%%    (3, ..., [], ...) -> 
%%       Fun(<last generator loop state>, ...);
%%    (3, ...., F, ...) -> % looping over values (using continuations)
%%       case F() of % get the next value by calling a continuation
%%           [Pattern | Val] -> 
%%               Fun(<next qualifier state>..., Val, ...);
%%           [_ | Val] -> 
%%               Fun(3, ..., Val, ...);
%%           [] ->
%%              Fun(<last generator loop state>, ...)
%%       end;
%%    (4, ...) -> % an sample filter
%%       case Filter of
%%           true -> Fun(<next qualifier state>, ...);
%%           false -> Fun(<last generator loop state>, ...)
%%       end;
%%    (5, ...) -> % a filter so simple that it could be used as a guard
%%       if 
%%          Guard -> Fun(<next qualifier state>, ...);
%%          true -> Fun(<last generator loop state>, ...)
%%       end
%% 
%% <last generator loop state> means state 0 if there is no last
%% generator. <initial value> is the evaluated list expression
%% (evaluated once only). Among the arguments indicated by ellipses
%% are all variables introduced in patterns and filters.
%%
%% transform/2 replaces each QLC expression (call to qlc:q/1) with a
%% qlc_lc record. The general case is that calling the fun stored in
%% the 'lc' field returns {qlc_v1, QFun, CodeF, Qdata} such that: QFun
%% is the above mentioned fun; CodeF is a fun returning the original
%% code for the template, every pattern, and every filter; Qdata is
%% the above mentioned table. There are two special cases when calling
%% the fun returns something else:
%%
%% - If the QLC has the form [Var || Var <- LE] and there are no
%%   options to qlc:q/2, a tuple {simple_v1, P, LEf, Line} is returned.
%%   The objects returned are the objects returned by the generator
%%   (calling LEf returns the objects generated by LE).
%%
%% - If the QLC has the form [T || P <- LE, F1, F2, ...] (if there is no Fi,
%%   'true' takes its place) a tuple {single_v1, QFun, CodeF, Qdata,
%%   MatchSpec, ColumnFun} is returned if ets:fun2ms(fun(P) when F1
%%   andalso F2 andalso ... -> P end) is a match specification or
%%   lookup can be used for finding key values. ColumnFun returns for
%%   every position (column) of the pattern P the constant values (or
%%   'false' if there are no constant values). MatchSpec is a match
%%   specification that can be used instead of the QLC, or
%%   'no_match_spec' if there no such match specification.

transform(Forms, State) ->
    IntroVars = intro_variables(Forms, State),
    AllVars = sets:from_list(ordsets:to_list(vars(Forms))),
    ?DEBUG("AllVars = ~p~n", [sets:to_list(AllVars)]),
    F1 = fun(QId, {generate,_,P,LE}, Foo, {GoI,SI}) ->
                 {{QId,GoI,SI,{gen,P,LE}},Foo,{GoI + 3, SI + 2}};
            (QId, F, Foo, {GoI,SI}) ->
                 {{QId,GoI,SI,{fil,F}},Foo,{GoI + 2,SI + 1}}
         end,
    TemplS = qlc:template_state(),
    GoState = {TemplS + 1, TemplS + 1},
    {ModifiedForms1,_} = qual_fold(F1, [], GoState, Forms, State),

    %% This is for info/2. QLC expressions in filters and the template
    %% are translated before the expression itself is translated.
    %% info/2 must not display the result of the translation, but the
    %% source code.
    {_,Source0} = qual_fold(fun(_QId, {generate,_,_P,_E}=Q, Dict, Foo) -> 
                                    {Q,Dict,Foo};
                               (QId, F, Dict, Foo) ->
                                    {F,dict:store(QId, F, Dict),Foo}
                            end, dict:new(), [], Forms, State),
    {_,Source} = qlc_mapfold(fun(Id, {lc,_L,E,_Qs}=LC, Dict) ->
                                     {LC,dict:store(Id, E, Dict)}
                             end, Source0, Forms, State),

    %% Unused variables introduced in filters are not optimized away.
    F2 = fun(Id, {lc,_L,E,Qs}, IntroVs0) ->
                 LcNo = get_lcid_no(Id),
                 LcL = get_lcid_line(Id),
                 [RL,Fun,Go,NGV,S0,RL0,Go0,AT] = 
                     aux_vars(['RL','Fun','Go','C','S0','RL0','Go0','AT'], 
                              LcNo, AllVars),
                 ?DEBUG("RL = ~p, Fun = ~p, Go = ~p~n", [RL, Fun, Go]),
                 F = fun({QId,GoI,SI,{gen,P,LE}}, 
                         {[{QId,IVs}|QsIVs],AllIVs0}) ->
                             GV = aux_var('C', LcNo, QId#qid.no, 1, AllVars),
                             GenIVs = [GV | IVs],
                             {{QId,{GenIVs,{{gen,P,LE,GV},GoI,SI}}},
                              {QsIVs,GenIVs ++ AllIVs0}};
                        ({QId,GoI,SI,{fil,F}}, 
                         {[{QId,IVs}|QsIVs],AllIVs0}) ->
                             {{QId,{IVs,{{fil,F},GoI,SI}}},
                              {QsIVs,IVs++AllIVs0}}
                     end,
                 {QCs,{IntroVs,AllIVs}} = 
                      lists:mapfoldl(F, {IntroVs0,[]}, Qs),

                 Cs0 = clauses(QCs, RL, Fun, Go, NGV, AllIVs, State),
                 L = no_compiler_warning(LcL),
                 Template = template(E, RL, Fun, Go, AT, L, AllIVs, State),
                 Fin = final(RL, AllIVs, L, State),
                 FunC = {'fun',L,{clauses,Fin ++ Template ++ Cs0}},
                 As0 = pack_args(abst_vars([S0, RL0, Fun, Go0 
                                            | replace(AllIVs, AllIVs, nil)], 
                                           L), L, State),
                 AsW = abst_vars([S0, RL0, Go0], L),
                 FunW = {'fun',L,{clauses,[{clause,L,AsW,[],
                                            [{match,L,{var,L,Fun},FunC},
                                             {call,L,{var,L,Fun},As0}]}]}},
                 {ok, OrigE0} = dict:find(Id, Source),
                 OrigE = undo_no_shadows(OrigE0),
                 QCode = qcode(OrigE, QCs, Source, L),
                 Qdata = qdata(QCs, L),
                 QOpt = ?A(undefined), %% no optimization data yet
                 LCTuple = 
                     case qlc_kind(OrigE, E, Qs, L, State) of
                         qlc ->
                             {tuple,L,[?A(qlc_v1),FunW,QCode,Qdata,QOpt]};
                         {simple, PL, LE, V} ->
                             Init = closure(LE, L),
                             {tuple,L,[?A(simple_v1),?A(V),Init,?I(PL)]};
                         {single, MS, ColumnFun} ->
                             {tuple,L,[?A(single_v1), FunW, QCode, 
                                       Qdata, QOpt, MS, ColumnFun]}
                     end,
                 LCFun = {'fun',L,{clauses,[{clause,L,[],[],[LCTuple]}]}},
                 {tuple,_,Fs0} = erl_parse:abstract(#qlc_lc{}, L),
                 Fs = set_field(#qlc_lc.lc, Fs0, LCFun),
                 {{tuple,L,Fs},IntroVs}
         end,
    {NForms,[]} = qlc_mapfold(F2, IntroVars, ModifiedForms1, State),
    display_forms(NForms),
    restore_line_numbers(NForms).

qlc_kind(OrigE, E, Qs, LcL, State) ->
    case {OrigE, qlc_kind2(undo_no_shadows(Qs))} of
        {{var,_,V}, {{var,PatternL,V},{atom,_,true}}} ->
            [{_,_,_,{gen,_,LE}} | _] = Qs,
            {simple, PatternL, LE, V};
        {_, {_P, _F}} ->
            {Pattern, Filter} = qlc_kind2(Qs),
            ColumnFun = column_fun(Pattern, Filter, LcL, State),
            case try_ms(E, Pattern, Filter, State) of
                no when ColumnFun =:= false ->
                    qlc;
                no ->
                    {single, {atom,0,no_match_spec}, ColumnFun};
                {ok, MS} when ColumnFun =:= false ->
                    {single, MS, {atom, 0, no_column_fun}};
                {ok, MS} ->
                    {single, MS, ColumnFun}
            end;
        _ ->
            qlc
    end.

qlc_kind2([{_,_,_,{gen,Pattern,_}}]) ->
    {Pattern, {atom,0,true}};
qlc_kind2([{_,_,_,{gen,Pattern,_}} | Filters0]) ->
    case lists:reverse(Filters0) of
        [{_,_,_,{fil,Filter}} | Filters] ->
            qlc_kind2(Filters, Pattern, Filter);
        _ ->
            []
    end;
qlc_kind2(_) ->
    [].

qlc_kind2([], Pattern, Filter) ->
    {Pattern, Filter};
qlc_kind2([{_,_,_,{fil,Filter}} | Filters], Pattern, Filter0) ->
    qlc_kind2(Filters, Pattern, {op,0,'andalso',Filter,Filter0});
qlc_kind2(_, _Pattern, _) ->
    [].

%% The column fun is to be used when there is a known key column or
%% indices. The argument is a column number and the return value is a
%% list of the values to look up to get all objects needed to evaluate
%% the filter. The order of the objects need not be the same as the
%% order the traverse fun would return them.

-define(PATTERN_VAR, top).
-define(ANON_VAR(N), N).

column_fun(Pattern0, Filter0, LcL, State) ->
    %% Makes test for equality simple:
    P1 = map_lines(fun(_L) -> 0 end, Pattern0), 
    Fil1 = map_lines(fun(_L) -> 0 end, Filter0), 
    P2 = expand_pattern_records(P1, State),
    {{P3, Fil}, _} = var_mapfold(fun({var, L, '_'}, N) ->
                                         {{var, L, ?ANON_VAR(N)}, N+1};
                                    (Var, N) -> {Var, N}
                                 end, 0, {P2,Fil1}),
    F0 = [],
    Imported = ordsets:subtract(vars(Fil), vars(P3)), % anonymous too
    %% Partial evaluation, among other things:
    {P4, F1} = element_calls(tuple2cons(P3), F0, Imported), 
    {P, F2} = match_in_pattern(P4, F1),
    F = unify('=:=', {var,0,?PATTERN_VAR}, P, F2, []),
    case filter(Fil, F, Imported, State) of
        [] ->
            false;
        Columns ->
            ColCls0 = 
                map(fun({Col, Vs0}) ->
                            Vs1 = lists:foldr
                                    (fun(V, A) -> {cons,0,V,A} 
                                     end, {nil,0}, Vs0),
                            Tag = case vars(Vs1) of
                                      Imp when length(Imp) > 0, % imported vars
                                               length(Vs0) > 1 ->
                                          usort_needed;
                                      _ ->
                                          values
                                  end,
                            Vs = {tuple,0,[{atom,0,Tag},Vs1]},
                            {clause,0,[erl_parse:abstract(Col)],[],[Vs]}
                    end, Columns) 
                ++ [{clause,0,[{var,0,'_'}],[],[{atom,0,false}]}],
            ColCls = map_lines(fun(_L) -> LcL end, ColCls0),
            {'fun', LcL, {clauses, ColCls}}
    end.

match_in_pattern({match, _, E10, E20}, F0) ->
    {E1, F1} = match_in_pattern(E10, F0),
    {E2, F} = match_in_pattern(E20, F1),
    {E1, unify('=:=', E1, E2, F, [])};
match_in_pattern(T, F0) when is_tuple(T) ->
    {L, F} = match_in_pattern(tuple_to_list(T), F0),
    {list_to_tuple(L), F};
match_in_pattern([E0 | Es0], F0) ->
    {E, F1} = match_in_pattern(E0, F0),
    {Es, F} = match_in_pattern(Es0, F1),
    {[E | Es], F};
match_in_pattern(E, F) ->
    {E, F}.

filter(_E, failed, _Imported, _State) ->
    [];
filter(E, F0, Imported, State) ->
    Fs = filter1(E, [F0], Imported, State),
    frames_to_columns(Fs, Imported).

%% One frame for each path through the and/or formula.
%%
%% "A xor B" is equal to "(A and not B) or (not A and B)". 
%% Ignoring "not B" and "not A" this is the same as "A or B"; 
%% "xor" can be handled just as "or".
filter1({op, _, Op, L0, R0}, Fs, Imp, S) when (Op =:= '=:='); (Op =:= '==') ->
    %% In the transformed code there are no records in lookup values
    %% because records are expanded away in prep_expr.
    lists:flatmap(fun(F0) ->
                          {L, F1} = prep_expr(L0, F0, S, Imp),
                          {R, F2} = prep_expr(R0, F1, S, Imp),
                          case unify(Op, L,  R , F2, Imp) of
                              failed -> [];
                              F -> [F]
                          end
                  end, Fs);
filter1({op, _, Op, L, R}, Fs, Imp, S) when Op =:= 'and'; Op =:= 'andalso' ->
    filter1(R, filter1(L, Fs, Imp, S), Imp, S);
filter1({op, _, Op, L, R}, Fs, Imp, S) when Op =:= 'or'; 
                                            Op =:= 'orelse';
                                            Op =:= 'xor' ->
    filter1(L, Fs, Imp, S) ++ filter1(R, Fs, Imp, S);
filter1(_E, Fs, _Imp, _S) ->
    Fs.

frames_to_columns([], _Imp) ->
    [];
frames_to_columns(Fs, Imp) ->
    Rs = map(fun(F) ->
                     RL = case deref({var, 0, ?PATTERN_VAR}, F) of
                              {cons_tuple, Cols} ->
                                  column_vars(Cols, 1, Imp);
                              _ ->
                                  []
                          end,
                     sofs:relation(RL) % possibly empty
             end, Fs),
    Ss = sofs:from_sets(Rs),
    %% D: columns matched (or compared) with constants in every frame (path)
    D = sofs:intersection(sofs:projection(fun(S) -> sofs:projection(1, S) end, 
                                          Ss)),
    Cs = sofs:restriction(sofs:relation_to_family(sofs:union(Ss)), D),
    sofs:to_external(Cs).

column_vars({cons,_,C,E}, Col, Imp) ->
    case is_const(C, Imp) of
        true ->
            [{Col, cons2tuple(C)} | column_vars(E, Col + 1, Imp)];
        false ->
            column_vars(E, Col + 1, Imp)
    end;
column_vars(_, _Col, _Imp) ->
    [].

%% Tuple tails are variables, never constants.
is_const(E, Imp) ->
    %% add_binding/5 has checked that E is normalisable. 
    [] == ordsets:to_list(ordsets:subtract(vars(E), Imp)).

prep_expr(E, F, S, Imp) ->
    element_calls(tuple2cons(expand_expr_records(E, S)), F, Imp).

%% cons_tuple is used for representing {V1, ..., Vi | TupleTail}.
element_calls({call,_,{remote,_,{atom,_,erlang},{atom,_,element}},
               [{integer,_,I},Term0]}, F0, Imp) when I > 0 ->
    VarI = {var, 0, make_ref()},
    TupleTail = {var, 0, make_ref()},
    Tuple = element_tuple(I, [VarI | TupleTail]),
    {Term, F} = element_calls(Term0, F0, Imp),    
    {VarI, unify('=:=', Tuple, Term, F, Imp)};
element_calls(T, F0, Imp) when is_tuple(T) ->
    {L, F} = element_calls(tuple_to_list(T), F0, Imp),
    {list_to_tuple(L), F};
element_calls([E0 | Es0], F0, Imp) ->
    {E, F1} = element_calls(E0, F0, Imp),
    {Es, F} = element_calls(Es0, F1, Imp),
    {[E | Es], F};
element_calls(E, F, _Imp) ->
    {E, F}.

element_tuple(1, Es) ->
    {cons_tuple, list2cons(Es)};
element_tuple(I, Es) ->
    element_tuple(I-1, [{var, 0, make_ref()} | Es]).

expand_pattern_records(P, State) ->
    E = {'case',0,{atom,0,true},[{clause,0,[P],[],[{atom,0,true}]}]},
    {'case',_,_,[{clause,0,[NP],_,_}]} = expand_expr_records(E, State),
    NP.

expand_expr_records(E, State) ->
    RecordDefs = State#state.records,
    Forms = RecordDefs ++ [{function,1,foo,0,[{clause,1,[],[],[E]}]}],
    {_,_,[F|_],_} = sys_pre_expand:module(Forms, []), 
    {function,_,foo,0,[{clause,_,[],[],[NE]}]} = F,
    NE.

unify(_Op, _E1, _E2, failed, _Imp) -> % contradiction
    failed;
unify(_Op, E, E, F, _Imp) ->
    F;
unify(Op, {var, _, _}=Var, E2, F, Imp) ->
    extend_frame(Op, Var, E2, F, Imp);
unify(Op, E1, {var, _, _}=Var, F, Imp) ->
    extend_frame(Op, Var, E1, F, Imp);
unify(Op, {cons_tuple, Es1}, {cons_tuple, Es2}, F, Imp) ->
    unify(Op, Es1, Es2, F, Imp);
unify(Op, {cons, _, L1, R1}, {cons, _, L2, R2}, F, Imp) ->
    unify(Op, R1, R2, unify(Op, L1, L2, F, Imp), Imp);
unify(Op, E1, E2, F, _Imp) ->
    %% This clause could just return F.
    case catch begin 
                   {ok, C1} = normalise(E1),
                   {ok, C2} = normalise(E2),
                   if 
                       Op =:= '=:=', C1 =:= C2 ->
                           F;
                       Op =:= '==', C1 == C2 ->
                           F;
                       true ->
                           failed
                   end end of
        {'EXIT', _} ->
            F; % ignored
        Reply ->
            Reply
    end.
%% Binaries are not handled at all.

-record(bind, {var, value}).

extend_frame(Op, Var, Value, F, Imp) ->
    case binding(Var, F) of
        #bind{var = Var, value = VarValue} ->
            unify(Op, VarValue, Value, F, Imp);
        false ->
            case Value of
                {var, _, _} ->
                    case binding(Value, F) of
                        #bind{var = Value, value = ValueValue} ->
                            unify(Op, Var, ValueValue, F, Imp);
                        false ->
                            add_binding(Op, Var, Value, F, Imp)
                    end;
                _ ->
                    add_binding(Op, Var, Value, F, Imp)
            end
    end.
    
%% If there is an integer (or float comparing equal to an integer) in
%% the value one has to be careful. One way would be to look up the
%% value both with the integer and with the float comparing equal to
%% the integer - then all objects that could possibly be answers are
%% filtered (with reasonable assumptions). But if integers occur
%% several times in the value all combinations have to be looked up,
%% and that could be just too many.
%%
%% If there are imported variables in the value one could assume at
%% compile time that they are not integers and check that assumption
%% at runtime. However, this implementation is much simpler: do not
%% bind the variable to the value if imported variables or integers
%% occur in the value. This will probably do.

add_binding(Op, Var, Value, F, Imp) ->
    case normalise(Value) of
        {ok, NValue} when Op =:= '==' ->
            case {ordsets:to_list(ordsets:intersection(vars(Value), Imp)), 
                  has_integer(NValue)} of
                {[], false} ->
                    add_binding(Var, Value, F);
                _ ->
                    F
            end;
        {ok, _} when Op =:= '=:=' ->
            add_binding(Var, Value, F);
        not_ok ->
            F
    end.

add_binding(Var, Value, F) ->
    case {occurs(Var, Value, F), Value} of
        {true, _} ->
            failed;
        {false, {var, _, Ref}} when is_reference(Ref) ->
            %% Push imported variables to the end of the binding chain
            %% in order to make is_const/2 work.
            [#bind{var = Value, value = Var} | F];
        {false, _} ->
            [#bind{var = Var, value = Value} | F]
    end.

normalise(E) ->
    %% Tuple tails are OK.
    case catch erl_parse:normalise(var2const(cons2tuple(E))) of
        {'EXIT', _} ->
            not_ok;
        C ->
            {ok, C}
    end.

occurs(V, V, _F) ->
    true;
occurs(V, {var, _, _} = Var, F) ->
    case binding(Var, F) of
        #bind{value = Value} ->
            occurs(V, Value, F);
        false ->
            false
    end;
occurs(V, T, F) when is_tuple(T) ->
    lists:any(fun(E) -> occurs(V, E, F) end, tuple_to_list(T));
occurs(V, [E | Es], F) ->
    occurs(V, E, F) or occurs(V, Es, F);
occurs(_V, _E, _F) ->
    false.

has_integer(I) when is_integer(I) ->
    true;
has_integer(F) when is_float(F) ->
    round(F) == F;
has_integer(T) when is_tuple(T) ->
    has_integer(tuple_to_list(T));
has_integer([E | Es]) ->
    has_integer(E) or has_integer(Es);
has_integer(_) ->
    false.

deref(E, F) ->
    var_map(fun(V) ->
                    case binding(V, F) of
                        #bind{value = Val} ->
                            deref(Val, F);
                        false ->
                            V
                    end
            end, E).

binding(V, [#bind{var = V}=B | _]) ->
    B;
binding(V, [_ | F]) ->
    binding(V, F);
binding(_V, _F) ->
    false.

tuple2cons({tuple, _, Es}) ->
    {cons_tuple, list2cons(tuple2cons(Es))};
tuple2cons(T) when is_tuple(T) ->
    list_to_tuple(tuple2cons(tuple_to_list(T)));
tuple2cons([E | Es]) ->
    [tuple2cons(E) | tuple2cons(Es)];
tuple2cons(E) ->
    E.

list2cons([E | Es]) ->
    {cons, 0, E, list2cons(Es)};
list2cons([]) ->
    {nil, 0};
list2cons(E) ->
    E.

%% Returns {..., Variable} if Variable is a tuple tail.
cons2tuple({cons_tuple, Es}) ->
    {tuple, 0, cons2list(Es)};
cons2tuple(T) when is_tuple(T) ->
    list_to_tuple(cons2tuple(tuple_to_list(T)));
cons2tuple([E | Es]) ->
    [cons2tuple(E) | cons2tuple(Es)];
cons2tuple(E) ->
    E.

cons2list({cons, _, L, R}) ->
    [cons2tuple(L) | cons2list(R)];
cons2list({nil, _}) ->
    [];
cons2list(E) -> % tuple tail (always a variable)
    [cons2tuple(E)].

%% Recognizes all QLCs on the form [T || P <- LE, F] such that
%% ets:fun2ms(fun(P) when F -> T end) is a match spec. This is OK with
%% the guard semantics implemented in filter/_ below. If one chooses
%% not to have guard semantics, affected filters will have to be
%% recognized and excluded here as well.
try_ms(E, P, Fltr, State) ->
    L = 1,
    Fun =  {'fun',L,{clauses,[{clause,L,[P],[[Fltr]],[E]}]}},
    Expr = {call,L,{remote,L,{atom,L,ets},{atom,L,fun2ms}},[Fun]},
    Form = {function,L,foo,0,[{clause,L,[],[],[Expr]}]},
    X = ms_transform:parse_transform(State#state.records ++ [Form], []),
    case catch 
        begin
            {function,L,foo,0,[{clause,L,[],[],[MS0]}]} = lists:last(X),
            MS = erl_parse:normalise(var2const(MS0)),
            XMS = ets:match_spec_compile(MS),
            true = is_binary(XMS), % never fails
            {ok, MS0} 
        end of
        {'EXIT', _} ->
            no;
        Reply ->
            Reply
    end.

set_field(Pos, Fs, Data) ->
    lists:sublist(Fs, Pos-1) ++ [Data] ++ lists:nthtail(Pos, Fs).

qdata([{QId,{_QIVs,{{gen,_P,LE,_GV},GoI,SI}}} | QCs], L) ->
    Init = closure(LE, L),
    %% Create qual_data (see qlc.erl):
    {cons,L,{tuple,L,[?I(QId#qid.no),?I(GoI),?I(SI),{tuple,L,[?A(gen),Init]}]},
     qdata(QCs, L)};
qdata([{QId,{_QIVs,{{fil,_F},GoI,SI}}} | QCs], L) ->
    %% Create qual_data (see qlc.erl):
    {cons,L,{tuple,L,[?I(QId#qid.no),?I(GoI),?I(SI),?A(fil)]},qdata(QCs, L)};
qdata([], L) ->
    {nil,L}.

%% The original code (in Source) is used for filters and the template
%% since the translated code can have QLC expressions and we don't
%% want them to be visible.
qcode(E, QCs, Source, L) ->
    F = fun({_,C}) -> 
                Bin = term_to_binary(C, [compressed]),
                {bin, L, [{bin_element, L, 
                           {string, L, binary_to_list(Bin)},
                           default, default}]}
        end,
    CL = map(F,lists:keysort(1, [{qlc:template_state(),E} | 
                                 qcode(QCs, Source)])),
    {'fun', L, {clauses, [{clause, L, [], [], [{tuple, L, CL}]}]}}.

qcode([{_QId, {_QIvs, {{gen,P,_LE,_GV}, GoI, _SI}}} | QCs], Source) ->
    [{GoI,undo_no_shadows(P)} | qcode(QCs, Source)];
qcode([{QId, {_QIVs, {{fil,_F}, GoI, _SI}}} | QCs], Source) ->
    {ok,OrigF} = dict:find(QId, Source),
    [{GoI,undo_no_shadows(OrigF)} | qcode(QCs, Source)];
qcode([], _Source) ->
    [].

closure(Code, L) ->
    {'fun',L,{clauses,[{clause,L,[],[],[Code]}]}}.

clauses([{QId,{QIVs,{QualData,GoI,S}}} | QCs], RL, Fun, Go, NGV, IVs, St) ->
    ?DEBUG("QIVs = ~p~n", [QIVs]),
    ?DEBUG("IVs = ~p~n", [IVs]),
    ?DEBUG("GoI = ~p, S = ~p~n", [GoI, S]),
    L = no_compiler_warning(get_lcid_line(QId#qid.lcid)),
    Cs = case QualData of
             {gen,P,_LE,GV} ->
                 generator(S, QIVs, P, GV, NGV, IVs, RL, Fun, Go, GoI, L, St);
             {fil,F} ->
                 filter(F, L, QIVs, S, RL, Fun, Go, GoI, IVs, St)
         end,
    Cs ++ clauses(QCs, RL, Fun, Go, NGV, IVs, St);
clauses([], _RL, _Fun, _Go, _NGV, _IVs, _St) ->
    [].

final(RL, IVs, L, State) ->
    IAs = replace(IVs, IVs, '_'),
    AsL = pack_args([?I(0) | abst_vars([RL, '_', '_'] ++ IAs, L)], L, State),
    Grd = [is_list_c(RL, L)],
    Rev = {call,L,{remote,L,?A(lists),?A(reverse)},[?V(RL)]},
    CL = {clause,L,AsL,[Grd],[Rev]},
    AsF = pack_args([?I(0) | abst_vars(['_', '_', '_'] ++ IAs, L)], L, State),
    CF = {clause,L,AsF,[],[?ABST_NO_MORE]},
    [CL, CF].

template(E, RL, Fun, Go, AT, L, IVs, State) ->
    I = qlc:template_state(), GoI = qlc:template_state(),
    ARL = {cons,L,E,abst_vars(RL, L)},
    Next = next(Go, GoI, L),
    As0 = abst_vars([RL, Fun, Go] ++ IVs, L),
    As = pack_args([?I(I) | As0], L, State),
    NAs = pack_args([Next, ARL] ++ abst_vars([Fun, Go] ++ IVs, L), L, State),
    Grd = [is_list_c(RL, L)],
    CL = {clause,L,As,[Grd],[{call,L,?V(Fun),NAs}]},

    %% Extra careful here or arguments will be lifted into a wide fun.
    F = case split_args([Next | As0], L, State) of
            {ArgsL, ArgsT} -> 
                Call = {call,L,?V(Fun),ArgsL++[{var,L,AT}]},
                {block,L,
                 [{match,L,{var,L,AT},ArgsT},
                  {'fun',L,{clauses,[{clause,L,[],[],[Call]}]}}]};
            FNAs ->
                {'fun',L,{clauses,[{clause,L,[],[],[{call,L,?V(Fun),FNAs}]}]}}
        end,
    CF = {clause,L,As,[],[?ABST_MORE(E, F)]},
    [CL,CF].

generator(S, QIVs, P, GV, NGV, IVs, RL, Fun, Go, GoI, L, State) ->
    ComAs = abst_vars([RL, Fun, Go], L),
    InitC = generator_init(S, L, GV, RL, Fun, Go, GoI, IVs, State),
    As = [?I(S + 1)| ComAs ++ abst_vars(replace(QIVs -- [GV], IVs, '_'), L)],

    MatchS = next(Go, GoI + 1, L),
    AsM0 = [MatchS | ComAs ++ abst_vars(replace([GV], IVs, NGV), L)],
    AsM = pack_args(AsM0, L, State),

    ContS = ?I(S + 1),
    QIVs__GV = QIVs -- [GV],
    Tmp = replace([GV], replace(QIVs__GV, IVs, nil), NGV),
    AsC = pack_args([ContS | ComAs ++ abst_vars(Tmp, L)], L, State),

    DoneS = next(Go, GoI, L),
    AsD0 = [DoneS | ComAs ++ abst_vars(replace(QIVs, IVs, nil), L)],
    AsD = pack_args(AsD0, L, State),

    CsL = generator_list(P, GV, NGV, As, AsM, AsC, AsD, Fun, L, State),
    CsF = generator_cont(P, GV, NGV, As, AsM, AsC, AsD, Fun, L, State),
    [InitC | CsL ++ CsF].
    
generator_init(S, L, GV, RL, Fun, Go, GoI, IVs, State) ->
    As0 = abst_vars([RL, Fun, Go] ++ replace([GV], IVs, '_'), L),
    As = pack_args([?I(S) | As0], L, State),
    Next = next(Go, GoI + 2, L),
    NAs = pack_args([?I(S + 1) | replace([?V('_')], As0, Next)], L, State),
    {clause,L,As,[],[{call,L,?V(Fun),NAs}]}.

generator_list(P, GV, NGV, As, AsM, AsC, AsD, Fun, L, State) ->
    As1 = pack_args(replace([?V(GV)], As, {cons,L,P,?V(NGV)}), L, State),
    As2 = pack_args(replace([?V(GV)], As, {cons,L,?V('_'),?V(NGV)}), L,State),
    As3 = pack_args(replace([?V(GV)], As, {nil,L}), L, State),
    CM = {clause,L,As1,[],[{call,L,?V(Fun),AsM}]},
    CC = {clause,L,As2,[],[{call,L,?V(Fun),AsC}]},
    CD = {clause,L,As3,[],[{call,L,?V(Fun),AsD}]},
    [CM, CC, CD].

generator_cont(P, GV, NGV, As0, AsM, AsC, AsD, Fun, L, State) ->
    As = pack_args(As0, L, State),
    CF1 = ?ABST_MORE(P, ?V(NGV)),
    CF2 = ?ABST_MORE(?V('_'), ?V(NGV)),
    CF3 = ?ABST_NO_MORE,
    CM = {clause,L,[CF1],[],[{call,L,?V(Fun),AsM}]},
    CC = {clause,L,[CF2],[],[{call,L,?V(Fun),AsC}]},
    CD = {clause,L,[CF3],[],[{call,L,?V(Fun),AsD}]},
    Cls = [CM, CC, CD],
    B = {'case',L,{call,L,?V(GV),[]},Cls},
    [{clause,L,As,[],[B]}].
    
filter(E, L, QIVs, S, RL, Fun, Go, GoI, IVs, State) ->
    IAs = replace(QIVs, IVs, '_'),
    As = pack_args([?I(S) | abst_vars([RL, Fun, Go] ++ IAs, L)], L, State),
    NAs = abst_vars([RL, Fun, Go] ++ IVs, L),
    TNext = next(Go, GoI + 1, L),
    FNext = next(Go, GoI, L),
    NAsT = pack_args([TNext | NAs], L, State),
    NAsF = pack_args([FNext | NAs], L, State),
    %% This is the "guard semantics" used in ordinary list
    %% comprehension: if a filter looks like a guard test, it returns
    %% 'false' rather than fails.
    Body = case erl_lint:is_guard_test(E, State#state.records) of
               true -> 
                   CT = {clause,L,[],[[E]],[{call,L,?V(Fun),NAsT}]},
                   CF = {clause,L,[],[[?A(true)]],[{call,L,?V(Fun),NAsF}]},
                   [{'if',L,[CT,CF]}];
               false -> 
                   CT = {clause,L,[?A(true)],[],[{call,L,?V(Fun),NAsT}]},
                   CF = {clause,L,[?A(false)],[],[{call,L,?V(Fun),NAsF}]},
                   [{'case',L,E,[CT,CF]}]
           end,
    [{clause,L,As,[],Body}].
    
pack_args(Args, L, State) ->
    case split_args(Args, L, State) of
        {ArgsL, ArgsT} ->
            ArgsL ++ [ArgsT];
        _ ->
            Args
    end.

split_args(Args, L, State) when length(Args) > State#state.maxargs ->
    {lists:sublist(Args, State#state.maxargs-1), 
     {tuple,L,lists:nthtail(State#state.maxargs-1, Args)}};
split_args(Args, _L, _State) ->
    Args.
    
%% Replace each element in IEs that are members of Es by R, keep all
%% other elements as they are.
replace(Es, IEs, R) ->
    map(fun(E) -> case lists:member(E, Es) of
                      true -> R; 
                      false -> E 
                  end 
        end, IEs).

is_list_c(V, L) ->
    {call,L,?A(is_list),[?V(V)]}.

next(Go, GoI, L) ->
    {call,L,?A(element),[?I(GoI),?V(Go)]}.

aux_vars(Vars, LcN, AllVars) ->
    map(fun(N) -> aux_var(N, LcN, 0, 1, AllVars) end, Vars).

aux_var(Name, LcN, QN, N, AllVars) ->
    aux_name(lists:concat([Name, LcN, '_', QN, '_']), N, AllVars).

aux_name(Name, N, AllNames) ->
    {VN, _} = aux_name1(Name, N, AllNames),
    VN.

aux_name1(Name, N, AllNames) ->
    SN = name_suffix(Name, N),
    case sets:is_element(SN, AllNames) of
        true -> aux_name1(Name, N + 1, AllNames);
        false -> {SN, N}
    end.

no_compiler_warning(Line) ->
    - abs(Line).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

qual_fold(Fun, GlobAcc0, Acc0, Forms, State) ->
    F = fun(Id, {lc,L,E,Qs0}, GA0) ->
                {Qs,GA,_NA} = qual_fold(Qs0, Fun, GA0, Acc0, Id, 1, []),
                {{lc,L,E,Qs},GA};
           (_Id, Expr, GA) ->
                {Expr,GA}
        end,
    qlc_mapfold(F, GlobAcc0, Forms, State).

qual_fold([Q0 | Qs], F, GA0, A0, Id, No, NQs) ->
    QId = qid(Id, No),
    {Q,GA,A} = F(QId, Q0, GA0, A0),
    qual_fold(Qs, F, GA, A, Id, No + 1, [Q | NQs]);
qual_fold([], _F, GA, A, _Id, _No, NQs) ->
    {lists:reverse(NQs),GA,A}.

qlc_mapfold(Fun, Acc0, Forms0, State) ->
    {Forms, A, _NNo} = qlcmf(Forms0, Fun, State#state.imp, Acc0, 1),
    {Forms, A}.

qlcmf([E0 | Es0], F, Imp, A0, No0) ->
    {E, A1, No1} = qlcmf(E0, F, Imp, A0, No0),
    {Es, A, No} = qlcmf(Es0, F, Imp, A1, No1),
    {[E | Es], A, No};
qlcmf(?QLC_Q(L1, L2, L3, L4, LC0, Os0), F, Imp, A0, No0) when length(Os0) < 2 ->
    {Os, A1, No1} = qlcmf(Os0, F, Imp, A0, No0),
    {LC, A2, No} = qlcmf(LC0, F, Imp, A1, No1), % nested...
    NL = make_lcid(L1, No),
    {T, A} = F(NL, LC, A2),
    {?QLC_Q(L1, L2, L3, L4, T, Os), A, No + 1};
qlcmf(?IMP_Q(L1, L2, LC0, Os0), F, Imp=true, A0, No0) when length(Os0) < 2 ->
    {Os, A1, No1} = qlcmf(Os0, F, Imp, A0, No0),
    {LC, A2, No} = qlcmf(LC0, F, Imp, A1, No1), % nested...
    NL = make_lcid(L, No),
    {T, A} = F(NL, LC, A2),
    {?IMP_Q(L1, L2, T, Os), A, No + 1};
qlcmf(T, F, Imp, A0, No0) when is_tuple(T) ->
    {TL, A, No} = qlcmf(tuple_to_list(T), F, Imp, A0, No0),
    {list_to_tuple(TL), A, No};
qlcmf(T, _F, _Imp, A, No) ->
    {T, A, No}.

vars(E) ->
    var_ufold(fun({var,_L,V}) -> V end, E).

var_ufold(F, E) ->
    ordsets:from_list(var_fold(F, [], E)).

var_fold(F, A, {var,_,V}=Var) when V =/= '_' ->
    [F(Var) | A];
var_fold(F, A, T) when is_tuple(T) ->
    var_fold(F, A, tuple_to_list(T));
var_fold(F, A, [E | Es]) ->
    var_fold(F, var_fold(F, A, E), Es);
var_fold(_F, A, _T) ->
    A.

no_shadows(Forms0, State) ->
    %% Variables that may shadow other variables are introduced in
    %% LCs and Funs. Such variables (call them SV, Shadowing
    %% Variables) are now renamed. Each (new) occurrence in a pattern
    %% is assigned an index (integer), unique in the file. 
    %%
    %% The state {LastIndex,ActiveVars,UsedVars,AllVars,Singletons}
    %% holds the last index used for each SV (LastIndex), the SVs in
    %% the current scope (ActiveVars), used SVs (UsedVars, the indexed
    %% name is the key), all variables occurring in the file
    %% (AllVars), and all singletons. If an SV is not used (that is,
    %% is a member of Singletons), it is replaced by '_' (otherwise a
    %% warning for unused variable would erroneously be emitted). If
    %% the indexed name of an SV occurs in the file, next index is
    %% tried (to avoid mixing up introduced names with existing ones).
    %%
    %% The original names of variables are kept in the line number
    %% position of the abstract code: {var, {OriginalName, L},
    %% NewName}. undo_no_shadows/1 re-creates the original code.
    AllVars = sets:from_list(ordsets:to_list(vars(Forms0))),
    ?DEBUG("nos AllVars = ~p~n", [sets:to_list(AllVars)]),
    VFun = fun(_Id, LC, Vs) -> nos(LC, Vs) end,
    LI = ets:new(?APIMOD,[]),
    UV = ets:new(?APIMOD,[]),
    D0 = dict:new(),
    S1 = {LI, D0, UV, AllVars, []},
    _ = qlc_mapfold(VFun, S1, Forms0, State),
    ?DEBUG("UsedIntroVars = ~p~n", [ets:match_object(UV, '_')]),
    Singletons = ets:select(UV, ets:fun2ms(fun({K,0}) -> K  end)),
    ?DEBUG("Singletons: ~p~n", [Singletons]),
    true = ets:delete_all_objects(LI),
    true = ets:delete_all_objects(UV),
    %% Do it again, this time we know which variables are singletons.
    S2 = {LI, D0, UV, AllVars, Singletons},
    {Forms,_} = qlc_mapfold(VFun, S2, Forms0, State),
    true = ets:delete(LI),
    true = ets:delete(UV),
    Forms.

nos([E0 | Es0], S0) ->
    {E, S1} = nos(E0, S0),
    {Es, S} = nos(Es0, S1),
    {[E | Es], S};
nos({'fun',L,{clauses,Cs}}, S) ->
    F = fun({clause,Ln,H0,G0,B0}) -> 
                {H, S1} = nos_pattern(H0, S),
                {[G, B], _} = nos([G0, B0], S1),
                {clause,Ln,H,G,B}
        end,
    NCs = map(F, Cs),
    {{'fun',L,{clauses,NCs}}, S};
nos({lc,L,E0,Qs0}, S) ->
    %% QLCs as well as LCs. It is OK to modify LCs as long as they
    %% occur within QLCs--the warning messages have already been found
    %% by compile_errors.
    F = fun({generate,Ln,P0,LE0}, QS0) -> 
                {LE, _} = nos(LE0, QS0),
                {P, QS} = nos_pattern(P0, QS0),
                {{generate,Ln,P,LE}, QS};
           (Filter, QS) -> 
                nos(Filter, QS)
        end,
    {Qs, S1} = lists:mapfoldl(F, S, Qs0),
    {E, _} = nos(E0, S1),
    {{lc,L,E,Qs}, S};
nos({var,L,V}=Var, {_LI,Vs,UV,_A,_Sg}=S) when V =/= '_' ->
    case used_var(V, Vs, UV) of
        {true, VN} -> {{var,{V,L},VN}, S};
        false -> {Var, S}
    end;
nos(T, S0) when is_tuple(T) ->
    {TL, S} = nos(tuple_to_list(T), S0),
    {list_to_tuple(TL), S};
nos(T, S) ->
    {T, S}.

nos_pattern(P, S) ->
    {T, NS, _} = nos_pattern(P, S, []),
    {T, NS}.

nos_pattern([P0 | Ps0], S0, PVs0) ->
    {P, S1, PVs1} = nos_pattern(P0, S0, PVs0),
    {Ps, S, PVs} = nos_pattern(Ps0, S1, PVs1),
    {[P | Ps], S, PVs};
nos_pattern({var,L,V}, {LI,Vs0,UV,A,Sg}, PVs0) when V =/= '_' ->
    {Name, Vs, PVs} = 
        case lists:keysearch(V, 1, PVs0) of
            {value, {V,VN}} -> 
                _ = used_var(V, Vs0, UV), 
                {VN, Vs0, PVs0};
            false -> 
                {VN, Vs1} = next_var(V, Vs0, A, LI, UV),
                N = case lists:member(VN, Sg) of
                        true -> '_';
                        false -> VN
                    end,
                {N, Vs1, [{V,VN} | PVs0]}
        end,
    {{var,{V,L},Name}, {LI,Vs,UV,A,Sg}, PVs};
nos_pattern(T, S0, PVs0) when is_tuple(T) ->
    {TL, S, PVs} = nos_pattern(tuple_to_list(T), S0, PVs0),
    {list_to_tuple(TL), S, PVs};
nos_pattern(T, S, PVs) ->
    {T, S, PVs}.

used_var(V, Vs, UV) ->
    case dict:find(V, Vs) of
        {ok,Value} ->
            VN = name_suffix(V, Value),
            _ = ets:update_counter(UV, VN, 1),
            {true, VN};
        error -> false
    end.

next_var(V, Vs, AllVars, LI, UV) ->
    NValue = case ets:lookup(LI, V) of
                 [{V, Value}] -> Value + 1;
                 [] -> 1
             end,
    true = ets:insert(LI, {V, NValue}),
    VN = name_suffix(V, NValue),
    case sets:is_element(VN, AllVars) of
        true -> next_var(V, Vs, AllVars, LI, UV);
        false -> true = ets:insert(UV, {VN, 0}),
                 NVs = dict:store(V, NValue, Vs),
                 {VN, NVs}
    end.

name_suffix(A, Suff) ->
    list_to_atom(lists:concat([A, Suff])).

undo_no_shadows(E) ->
    var_map(fun undo_no_shadows1/1, E).

undo_no_shadows1({var, {V, VL}, _}) ->
    undo_no_shadows1({var, VL, V});
undo_no_shadows1({var, _, _}=Var) ->
    Var.

restore_line_numbers(E) ->
    var_map(fun restore_line_numbers1/1, E).

restore_line_numbers1({var, {_, VL}, V}) ->
    restore_line_numbers1({var, VL, V});
restore_line_numbers1({var, L, _}=Var) when is_integer(L) ->
    Var.

-define(MAX_NUM_OF_LINES, 23). % assume max 1^23 lines (> 8 millions)

%% QLC expression identifier. 
%% The first one encountered in the file has No=1.

make_lcid(Line, No) when is_integer(Line), is_integer(No), No > 0 ->
    (No bsl ?MAX_NUM_OF_LINES) + Line.

is_lcid(Id) ->
  is_integer(Id) and (Id > (1 bsl ?MAX_NUM_OF_LINES)).

get_lcid_no(Id) ->
    Id bsr ?MAX_NUM_OF_LINES.

get_lcid_line(Id) ->
    Id band ((1 bsl ?MAX_NUM_OF_LINES) - 1).

qid(LCId, No) ->
    #qid{no = No, lcid = LCId}.

abst_vars([V | Vs], L) ->
    [abst_vars(V, L) | abst_vars(Vs, L)];
abst_vars([], _L) ->
    [];
abst_vars(nil, L) ->
    {nil,L};
abst_vars(V, L) ->
    {var,L,V}.

embed_vars(Vars, L) ->
    embed_expr({tuple,L,Vars}, L).

%% -> [Expr || _ <- []] on abstract format.
embed_expr(Expr, L) ->
    {lc,L,Expr,[{generate,L,{var,L,'_'},{nil,L}}]}.

var2const(E) ->
    var_map(fun({var, L, V}) -> {atom, L, V} end, E).

var_map(F, {var, _, _}=V) ->
    F(V);
var_map(F, T) when is_tuple(T) ->
    list_to_tuple(var_map(F, tuple_to_list(T)));
var_map(F, [E | Es]) ->
    [var_map(F, E) | var_map(F, Es)];
var_map(_F, E) ->
    E.

var_mapfold(F, A, {var, _, _}=V) ->
    F(V, A);
var_mapfold(F, A0, T) when is_tuple(T) ->
    {L, A} = var_mapfold(F, A0, tuple_to_list(T)),
    {list_to_tuple(L), A};
var_mapfold(F, A0, [E0 | Es0]) ->
    {E, A1} = var_mapfold(F, A0, E0),
    {Es, A} = var_mapfold(F, A1, Es0),
    {[E | Es], A};
var_mapfold(_F, A, E) ->
    {E, A}.

family(L) ->
    sofs:relation_to_family(sofs:relation(L)).

-ifdef(debug).
display_forms(Forms) ->
    io:format("Forms ***~n"),
    lists:foreach(fun(Form) ->
                          io:format("~s~n", [catch erl_pp:form(Form)])
                  end, Forms),
    io:format("End Forms ***~n").
-else.
display_forms(_) ->
    ok.
-endif.

