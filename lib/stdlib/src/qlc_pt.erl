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

-import(lists, [append/1, flatmap/2, flatten/1, keysearch/3, map/2, sort/1]).

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

-record(state, {imp, maxargs, records, xwarnings = []}).

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

-define(QLC_FILE, qlc_current_file).

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
        {[],[],Warnings0} ->
            {NewForms, State1} = transform(FormsNoShadows, State),
            Warnings = Warnings0 ++ State1#state.xwarnings,
            {[],WForms} = no_duplicates(NewForms, [], Warnings, Options),
            WForms ++ NewForms;
        {E0,Errors,Warnings} ->
            {EForms,WForms} = no_duplicates(Forms, E0++Errors, Warnings, Options),
            EForms ++ WForms ++ Forms
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
            {NewForms,_State1} = transform(FormsNoShadows, State),
            {function,L,bar,L,[{clause,L,As,[],[NF]}]} = lists:last(NewForms),
            {ok,NF};
        {E0,Errors,_Warnings} when WithLintErrors ->
            {not_ok,mforms(error, E0 ++ Errors)};
        {E0,Errors0,_Warnings} ->
            [{error,Reason} | _] = mforms(error, E0++Errors0),
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
    Es = mforms2(error, Errors) -- mforms2(error, Es1),
    Ws = mforms2(warning, Warnings) -- mforms2(warning, Ws1),
    {Es,Ws}.

mforms(Tag, L) ->
    sort([{Tag,M} || {_File,Ms} <- L, M <- Ms]).

mforms2(Tag, L) ->
    Line = 0,
    ML = flatmap(fun({File,Ms}) ->
                         [[{attribute,Line,file,{File,Line}}, {Tag,M}] || 
                             M <- Ms]
                 end, sort(L)),
    flatten(sort(ML)).

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
                {A,[{get(?QLC_FILE), [E]} | Es]}
        end,
    {_,E0} = qlc_mapfold(F, [], Forms, State),
    E0.

tag_lines(E, No) ->
    map_lines(fun(Id) -> 
                      case is_lcid(Id) of
                          true -> Id;
                          false -> make_lcid(Id, No)
                      end
              end, E).

map_lines(F, E) ->
    erl_lint:modify_line(E, F).

tagged_messages(MsL) ->
    [{File,
      [{get_lcid_line(Id),Mod,untag(T)} || {Id,Mod,T} <- Ms,
                                           is_lcid(Id)]}
     || {File,Ms} <- MsL]
    ++
    [{File,[{Line,?APIMOD,{used_generator_variable,V}}]} 
       || {_, Ms} <- MsL, 
          {{extra,Line,File,V},erl_lint,{unbound_var,_}} <- Ms].

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
            flatmap(fun({_File,Es}) -> Es end, Errors)
    end.

compile_forms(Forms0, Options) ->
    Forms1 = no_qlc_transform(Forms0),
    Forms = [F || F <- Forms1, element(1, F) =/= eof] ++ [{eof,999999}],
    try 
        case compile:forms(Forms, compile_options(Options)) of
            {ok, _ModName, Ws0} ->
                {[], Ws0};
            {error, Es0, Ws0} -> 
                {Es0, Ws0}
        end
    catch _:_ ->
        %% The compiler is not available. Use the linter instead.
        case erl_lint:module(Forms, Options) of
            {ok, Warnings} ->
                {[], Warnings};
            {error, Errors, Warnings} ->
                {Errors, Warnings}
        end
    end.

no_qlc_transform([{attribute, _, compile, {parse_transform,?APIMOD}} | Fs]) ->
    no_qlc_transform(Fs);
no_qlc_transform([F | Fs]) ->
    [F | no_qlc_transform(Fs)];
no_qlc_transform([]) ->
    [].

compile_options(Options) ->
    No = [report,report_errors,report_warnings,'P','E'],
    [strong_validation,return | [O || O <- Options, not lists:member(O, No)]].

%% In LCs it is possible to use variables introduced in filters and
%% generator patterns in the right hand side of generators (ListExpr),
%% but in QLCs this is not allowed. 
%%
%% A brand new function is returned such that there is one expression
%% for each ListExpr. The expression mentions all introduced variables
%% occurring in ListExpr. Running the function through the compiler
%% yields error messages for erroneous use of introduced variables.
%% The messages have the form
%% {{extra,LineNo,File,Var},Module,{unbound_var,V}}, where Var is the
%% original variable name and V is the name invented by no_shadows/2.
%%
used_genvar_check(Forms, State) ->
    F = fun(QId, {generate, Ln, _P, LE}=Q, {QsIVs0, Exprs0}, IVsSoFar0) ->
                F = fun({var, _, V}=Var) -> 
                            {var, L, OrigVar} = undo_no_shadows(Var),
                            {var, {extra, L, get(?QLC_FILE), OrigVar}, V} 
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
%% represented by funs (the table is further processed at runtime).
%% The separation into a fun and a table makes it possible to
%% rearrange qualifiers while keeping the speed offered by compiled
%% code, and to run the LEs before evaluation of the QLC (and possibly
%% modify the LEs should that be necessary). Only when doing a fast
%% join are qualifiers rearranged.
%%
%% Extra generators (and clauses) are inserted for possible fast join
%% operations. The list expression for such a generator has the form
%% {join, Op, QualifierNumber1, QualifierNumber2, PatternFilter1,
%% PatternFilter2, PatternConstants1, PatternConstants2} (it is not a
%% fun). Join generators are ignored at runtime unless a fast join is
%% possible, in which case they replace other generators. See also
%% qlc.erl.
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
%% Note: the fun must not return a fun if it is to be called by
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
%%              Fun(<last generator loop state>, ...);
%%           T -> % returned immediately, typically an error tuple
%%              T
%%       end;
%%    (4, ...) -> % a sample filter
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
%%   'no_match_spec' if there is no such match specification.
%%   [This is R10B only. In R11B more than one generator of a QLC
%%    expression may have match specifications.]

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
    {ModifiedForms1,_} = 
        qual_fold(F1, [], GoState, Forms, State),

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
    F2 = fun(Id, {lc,_L,E,Qs}, {IntroVs0,XWarn0}) ->
                 LcNo = get_lcid_no(Id),
                 LcL = get_lcid_line(Id),
                 [RL,Fun,Go,NGV,S0,RL0,Go0,AT,Err] = 
                     aux_vars(['RL','Fun','Go','C','S0','RL0','Go0','AT','E'],
                              LcNo, AllVars),
                 ?DEBUG("RL = ~p, Fun = ~p, Go = ~p~n", [RL, Fun, Go]),
                 {IntroVs, RestIntroVs} = lists:split(length(Qs), IntroVs0),
                 IntroVs_Qs = lists:zip(IntroVs, Qs),
                 F = fun({{QId,IVs}, {QId,GoI,SI,{gen,P,LE}}}, AllIVs0) ->
                             GV = aux_var('C', LcNo, QId#qid.no, 1, AllVars),
                             GenIVs = [GV | IVs],
                             {{QId,{GenIVs,{{gen,P,LE,GV},GoI,SI}}},
                              GenIVs ++ AllIVs0};
                        ({{QId,IVs}, {QId,GoI,SI,{fil,F}}}, AllIVs0) ->
                             {{QId,{IVs,{{fil,F},GoI,SI}}},
                              IVs++AllIVs0}
                     end,
                 {QCs, AllIVs} = lists:mapfoldl(F, [], IntroVs_Qs),

                 Dependencies = qualifier_dependencies(Qs, IntroVs),
                 L = no_compiler_warning(LcL),
                 {ColumnConstants, SizeInfo, ExtraConsts} = 
                      constants_and_sizes(Qs, E, Dependencies, AllIVs, State),
                 {JoinInfo, XWarn} = 
                     join_kind(Qs, LcL, AllIVs, Dependencies, State),
                 JQs = join_quals(JoinInfo, QCs, L, LcNo, ExtraConsts, AllVars),
                 XQCs = QCs ++ JQs,
                 Cs0 = clauses(XQCs, RL, Fun, Go, NGV, Err, AllIVs, State),
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
                 QCode = qcode(OrigE, XQCs, Source, L),
                 Qdata = qdata(XQCs, L),
                 TemplateInfo = 
                     template_columns(Qs, E, AllIVs, Dependencies, State),
                 %% ExtraConsts should be used by match_spec_quals.
                 MSQs = match_spec_quals(E, Dependencies, Qs, State),
                 Opt = opt_info(TemplateInfo, SizeInfo,
                                JoinInfo, ColumnConstants, MSQs, L),
                 LCTuple = 
                     case qlc_kind(OrigE, Qs) of
                         qlc ->
                             {tuple,L,[?A(qlc_v1),FunW,QCode,Qdata,Opt]};
                         {simple, PL, LE, V} ->
                             Init = closure(LE, L),
                             {tuple,L,[?A(simple_v1),?A(V),Init,?I(PL)]}
                     end,
                 LCFun = {'fun',L,{clauses,[{clause,L,[],[],[LCTuple]}]}},
                 {tuple,_,Fs0} = abstr(#qlc_lc{}, L),
                 Fs = set_field(#qlc_lc.lc, Fs0, LCFun),
                 {{tuple,L,Fs},{RestIntroVs,XWarn0++XWarn}}
         end,
    {NForms,{[],XW}} = qlc_mapfold(F2, {IntroVars,[]}, ModifiedForms1, State),
    display_forms(NForms),
    {restore_line_numbers(NForms), State#state{xwarnings = XW}}.

join_kind(Qs, LcL, AllIVs, Dependencies, State) ->
    {EqualCols2, EqualColsN} = equal_columns(Qs, AllIVs, Dependencies, State),
    {MatchCols2, MatchColsN} = eq_columns(Qs, AllIVs, Dependencies, State),
    Tables = lists:usort([T || C <- EqualCols2, {T,_} <- C]
                         ++ [T || C <- EqualCols2, T <- C, is_integer(T)]),
    if 
        EqualColsN =/= []; MatchColsN =/= [] -> 
            {[], 
             [{get(?QLC_FILE),[{abs(LcL),?APIMOD,too_complex_join}]}]};
        EqualCols2 =:= [], MatchCols2 =:= [] ->
            {[], []};
        length(Tables) > 2 -> 
            {[], 
             [{get(?QLC_FILE),[{abs(LcL),?APIMOD,too_many_joins}]}]};
        EqualCols2 =:= MatchCols2 ->
            {EqualCols2, []};
        true -> 
            {{EqualCols2, MatchCols2}, []}
    end.

qlc_kind(OrigE, Qs) ->
    {OrigFilterData, OrigGeneratorData} = qual_data(undo_no_shadows(Qs)),
    OrigAllFilters = filters_as_one(OrigFilterData),
    {_FilterData, GeneratorData} = qual_data(Qs),
    case {OrigE, OrigAllFilters, OrigGeneratorData} of
        {{var,_,V}, {atom,_,true}, [{_,{gen,{var,PatternL,V},_LE}}]} ->
            [{_,{gen,_,LE}}] = GeneratorData,
            {simple, PatternL, LE, V}; % V is for info()
        _ ->
            qlc
    end.

-define(TNO, 0).
-define(TID, #qid{lcid = template, no = ?TNO}).

opt_info(TemplateInfo, Sizes, JoinInfo, ColumnConstants0, MSQs, L) ->
    SzCls = [{clause,L,[?I(C)],[],[?I(Sz)]} || {C,Sz} <- sort(Sizes)]
            ++ [{clause,L,[?V('_')],[],[?A(undefined)]}],
    S = [{size, {'fun', L, {clauses, SzCls}}}],
    J = case JoinInfo of [] -> []; _ -> [{join, abstr(JoinInfo, L)}] end,
    %% Superfluous clauses may be emitted:
    TCls0 = append(
              [[{clause,L,[abstr(Col, L),EqType],[],
                 [abstr(TemplCols, L)]} ||
                   {Col,TemplCols} <- TemplateColumns]
               || {EqType, TemplateColumns} <- TemplateInfo]),
    TCls = sort(TCls0) ++ [{clause,L,[?V('_'),?V('_')],[],[{nil,L}]}],
    T = [{template, {'fun', L, {clauses, TCls}}}],

    %% The template may also have a constant function (IdNo = 0).
    %% Only constant template columns are interesting.
    ColumnConstants = [CC || {{IdNo,_Col},Const,_FilNs}=CC <- ColumnConstants0,
                             (IdNo =/= ?TNO) or (length(Const) =:= 1)],
    Ns = lists:usort([IdNo || {{IdNo,_Col},_Const,_FilNs} <- ColumnConstants]),
    CCs = [{clause,L,[?I(IdNo)],[],[column_fun(ColumnConstants, IdNo, L)]}
           || IdNo <- Ns]
         ++ [{clause,L,[?V('_')],[],[?A(no_column_fun)]}],
    C = [{constants,{'fun',L,{clauses,CCs}}}],

    ConstCols = [{IdNo,Col} || {{IdNo,Col},[_],_FilNs} <- ColumnConstants],
    ConstColsFamily = family_list(ConstCols),
    NSortedCols0 = [{IdNo,hd(lists:seq(1, length(Cols)+1)--Cols)} ||
                       {IdNo,Cols} <- ConstColsFamily],
    NCls = [{clause,L,[?I(IdNo)],[],[?I(N-1)]} ||
               {IdNo,N} <- NSortedCols0, N > 0]
           ++ [{clause,L,[?V('_')],[],[?I(0)]}],
    N = [{n_leading_constant_columns,{'fun',L,{clauses,NCls}}}],

    ConstCls = [{clause,L,[?I(IdNo)],[],[abstr(Cols,L)]} ||
                   {IdNo,Cols} <- ConstColsFamily] 
               ++ [{clause,L,[?V('_')],[],[{nil,L}]}],
    CC = [{constant_columns,{'fun',L,{clauses,ConstCls}}}],

    MSCls = [{clause,L,[?I(G)],[],[{tuple,L,[MS,abstr(Fs,L)]}]} ||
                {G,MS,Fs} <- MSQs]
          ++ [{clause,L,[?V('_')],[],[?A(undefined)]}],
    MS = [{match_specs, {'fun',L,{clauses,MSCls}}}],

    Cls = [{clause,L,[?A(Tag)],[],[V]} || 
              {Tag,V} <- append([J, S, T, C, N, CC, MS])]
          ++ [{clause,L,[?V('_')],[],[?A(undefined)]}],
    {'fun', L, {clauses, Cls}}.

abstr(Term, Line) ->
    erl_parse:abstract(Term, Line).

%% Extra generators are introduced for join.
join_quals(JoinInfo, QCs, L, LcNo, ExtraConstants, AllVars) ->
    {LastGoI, LastSI} =
        lists:foldl(fun({_QId,{_QIVs,{{fil,_},GoI,SI}}}, 
                        {GoI0, _SI0}) when GoI >= GoI0 ->
                            {GoI + 2, SI + 1};
                       ({_QId,{_QIVs,{{gen,_,_,_},GoI,SI}}}, 
                        {GoI0, _SI0}) when GoI >= GoI0 ->
                            {GoI + 3, SI + 2};
                       (_, A) ->
                            A
                    end, {0, 0}, QCs),
    LastQId = lists:max([QId || {QId,{_QIVs,{_Q,_GoI,_SI}}} <- QCs]),
    %% Only two tables for the time being.
    %% The join generator re-uses the generator variable assigned to
    %% the first of the two joined generators. Its introduced variables
    %% are the variables introduced by any of the two joined generators.
    %% Its abstract code is a pair of the joined generators patterns.
    QNums = case JoinInfo of
                {EqualCols, MatchCols} ->
                    EQs = join_qnums(EqualCols),
                    MQs = join_qnums(MatchCols),
                    [{Q1,Q2,'=:='} || {Q1,Q2} <- MQs] ++
                        [{Q1,Q2,'=='} || {Q1,Q2} <- EQs -- MQs];
                EqualCols ->
                    [{Q1,Q2,'=='} || {Q1,Q2} <- join_qnums(EqualCols)]
            end,
    LD = map(fun({Q1, Q2, Op}) ->
                     [{QId1,P1,GV1,QIVs1}] = 
                         [{QId,P,GV,QIVs} || 
                             {QId,{QIVs,{{gen,P,_,GV},_GoI,_SI}}} <- QCs, 
                             QId#qid.no =:= Q1],
                     [{QId2,P2,QIVs2}] = 
                         [{QId,P,QIVs--[GV]} || 
                             {QId,{QIVs,{{gen,P,_,GV},_,_}}} <- QCs,
                             QId#qid.no =:= Q2],
                     {QId1,Op,P1,GV1,QIVs1++QIVs2,QId2,P2}
             end, lists:usort(QNums)),
    Aux = abst_vars(aux_vars(['F','H','O','C'], LcNo, AllVars), L),
    F = fun({QId1,Op,P1,GV1,QIVs,QId2,P2}, {QId,GoI,SI}) ->
                AP1 = anon_pattern(P1),
                AP2 = anon_pattern(P2),
                Cs1 = join_handle_constants(QId1, ExtraConstants),
                Cs2 = join_handle_constants(QId2, ExtraConstants),
                H1 = join_handle(AP1, L, Aux, Cs1),
                H2 = join_handle(AP2, L, Aux, Cs2),
                Join = {join,Op,QId1#qid.no,QId2#qid.no,H1,H2,Cs1,Cs2},
                G = {NQId=QId#qid{no = QId#qid.no + 1},
                     {QIVs,{{gen,{cons,L,P1,P2},Join,GV1},GoI,SI}}},
                A = {NQId, GoI + 3, SI + 2},
                {G, A}
        end,
    {Qs, _} = lists:mapfoldl(F, {LastQId, LastGoI, LastSI}, LD),
    Qs.

join_qnums(Cols) ->
    [{Q1, Q2} || [{Q1,_C1}, {Q2,_C2}] <- Cols].

%% Variables occurring only once are replaced by '_'.
anon_pattern(P) ->
    MoreThanOnce = lists:usort(occ_vars(P) -- vars(P)),
    {AP, foo} = var_mapfold(fun({var, L, V}, A) ->
                                    case lists:member(V, MoreThanOnce) of
                                        true -> 
                                            {{var, L, V}, A};
                                        false ->
                                            {{var, L, '_'}, A}
                                    end
                            end, foo, P),
    AP.

%% Creates a handle that filters the operands of merge join using the
%% pattern. It is important that objects that do not pass the pattern
%% are filtered out because the columns of the pattern are inspected
%% in order to determine if key-sorting the operands can be avoided.
%% 
%% No objects will be filtered out if the pattern is just a variable.
join_handle(AP, L, [F, H, O, C], Constants) ->
    case {AP, Constants} of
        {{var, _, _}, []} ->
            {'fun',L,{clauses,[{clause,L,[H],[],[H]}]}};
        _ ->
            G0 = [begin
                      Call = {call,0,{atom,0,element},[{integer,0,Col},O]},
                      list2op([{op,0,'=:=',Con,Call} || Con <- Cs], 'or')
                  end || {Col,Cs} <- Constants],
            G = if G0 =:= [] -> G0; true -> [G0] end,
            CC1 = {clause,L,[AP],G,[{cons,L,O,closure({call,L,F,[F,C]},L)}]},
            CC2 = {clause,L,[?V('_')],[],[{call,L,F,[F,C]}]},
            Case = {'case',L,O,[CC1,CC2]},
            Cls = [{clause,L,[?V('_'),{nil,L}],[],[{nil,L}]},
                   {clause,L,[F,{cons,L,O,C}],[],[Case]},
                   {clause,L,[F,C],[[{call,L,?A(is_function),[C]}]],
                    [{call,L,F,[F,{call,L,C,[]}]}]},
                   {clause,L,[?V('_'),C],[],[C]}],
            Fun = {'fun', L, {clauses, Cls}},
            {'fun',L,{clauses,[{clause,L,[H],[],[{match,L,F,Fun}, 
                                                 closure({call,L,F,[F,H]}, 
                                                         L)]}]}}
    end.

join_handle_constants(QId, ExtraConstants) ->
    IdNo = QId#qid.no,
    case lists:keysearch(IdNo, 1, ExtraConstants) of
        {value, {IdNo, Consts}} -> Consts;
        false -> []
    end.

%%% By the term "imported variable" is meant a variable that is bound
%%% outside (before) the QLC expression. Perhaps "parameter" would be
%%% a more suitable name.

%% The column fun is to be used when there is a known key column or
%% indices. The argument is a column number and the return value is a
%% list of the values to look up to get all objects needed to evaluate
%% the filter. The order of the objects need not be the same as the
%% order the traverse fun would return them.

column_fun(Columns, QualifierNumber, LcL) ->
    ColCls0 = 
        [begin
             true = Vs0 =/= [], % at least one value to look up
             Vs1 = list2cons(Vs0),
             Fils1 = {tuple,0,[{atom,0,FTag},
                               lists:foldr
                                   (fun(F, A) -> {cons,0,{integer,0,F},A} 
                                    end, {nil,0}, Fils)]},
             Tag = case ordsets:to_list(vars(Vs1)) of
                       Imp when length(Imp) > 0, % imported vars
                                length(Vs0) > 1 ->
                           usort_needed;
                       _ ->
                           values
                   end,
             Vs = {tuple,0,[{atom,0,Tag},Vs1,Fils1]},
             {clause,0,[erl_parse:abstract(Col)],[],[Vs]}
         end ||
            {{CIdNo,Col}, Vs0, {FTag,Fils}} <- Columns,
            CIdNo =:= QualifierNumber]
        ++ [{clause,0,[{var,0,'_'}],[],[{atom,0,false}]}],
    ColCls = set_line(ColCls0, LcL),
    {'fun', LcL, {clauses, ColCls}}.

%% Tries to find columns of the template that (1) are equal to (or
%% match) or (2) match columns of the patterns of the generators. The
%% results are to be used only for determining which columns are
%% sorted. The template can be handled very much like a generator
%% pattern (the variables are not fresh, though). As in filters calls
%% like element(I, T) are recognized.
%% -> [{EqType,Equal | Match}]
%% Equal = Match = TemplateColumns
%% EqType = abstract code for {_ | '==' | '=:='}
%% TemplateColumns = [{Column,Integers}]
%% Column = {QualifierNumber,ColumnNumber}}

template_columns(Qs0, E0, AllIVs, Dependencies, State) ->
    E = expand_expr_records(pre_expand(E0), State),
    TemplateAsPattern = template_as_pattern(E),
    Qs = [TemplateAsPattern | Qs0],
    EqualColumns = equal_columns2(Qs, AllIVs, Dependencies, State),
    MatchColumns = eq_columns2(Qs, AllIVs, Dependencies, State),
    Equal = template_cols(EqualColumns), 
    Match = template_cols(MatchColumns),
    L = 0,
    if 
        Match =:= Equal -> 
            [{?V('_'), Match}];
        true -> 
            [{?A('=='), Equal}, {?A('=:='), Match}]
    end.

template_cols(ColumnClasses) ->
    sort([{{IdNo,Col}, lists:usort(Cs)} ||
             Class <- ColumnClasses,
             {IdNo,Col} <- Class,
             IdNo =/= ?TNO,
             [] =/= (Cs = [C || {?TNO,C} <- Class])]).

template_as_pattern(E) ->
    P = simple_template(E),
    {?TID,foo,foo,{gen,P,{nil,0}}}.

simple_template({call,L,{remote,_,{atom,_,erlang},{atom,_,element}}=Call,
                 [{integer,_,I}=A1,A2]}) when I > 0 ->
    %% This kludge is known by pattern/5 below.
    {call, L, Call, [A1, simple_template(A2)]};
simple_template({var, _, _}=E) ->
    E;
simple_template({tuple, L, Es}) ->
    {tuple, L, map(fun simple_template/1, Es)};
simple_template({cons, L, H, T}) ->
    {cons, L, simple_template(H), simple_template(T)};
simple_template(E) ->
    case catch erl_parse:normalise(E) of
        {'EXIT', _} -> unique_var();
        _ -> E
    end.

%% -> [{QId,[QId']}].
%% Qualifier QId (a filter) uses variables introduced in QId'.
qualifier_dependencies(Qualifiers, IntroVs) ->
    Intro = sofs:relation([{IV,QId} || {QId,IVs} <- IntroVs, IV <- IVs]),
    {FilterData, _} = qual_data(Qualifiers),
    Used = sofs:relation([{QId,UV} ||
                             {QId,{fil,F}} <- FilterData,
                             UV <- vars(F)]),
    Depend = sofs:strict_relation(sofs:relative_product(Used, Intro)),
    G = sofs:family_to_digraph(sofs:relation_to_family(Depend)),
    Dep0 = [{V,digraph_utils:reachable_neighbours([V], G)} || 
               V <- digraph:vertices(G)],
    true = digraph:delete(G),
    FilterIds = sofs:set(filter_ids(Qualifiers)),
    Dep1 = sofs:restriction(sofs:family(Dep0), FilterIds),
    NoDep = sofs:constant_function(FilterIds, sofs:empty_set()),
    sofs:to_external(sofs:family_union(Dep1, NoDep)).

filter_ids(Qualifiers) ->
    {FilterData, _} = qual_data(Qualifiers),
    [QId || {QId,_} <- FilterData].

%% -> [{QualifierNumber,MatchSpec,[QualifierNumber']}
%% The qualifiers [QualifierNumber'] are filters (F1, ..., Fn) that
%% depend on QualifierNumber (a generator Pattern <- LE) only.
%% MatchSpec is the match specification for [Pattern' || Pattern <- LE,
%% F1, ..., Fn], where Pattern' is Template if all qualifiers can be 
%% replaced by one match specification, otherwise a modified Pattern.
match_spec_quals(Template, Dependencies, Qualifiers, State) ->
    {FilterData, GeneratorData} = qual_data(Qualifiers),
    NoFilterGIds = [GId || {GId,_} <- GeneratorData] 
                   -- lists:flatmap(fun({_,GIds}) -> GIds end, Dependencies),
    Filters = filter_list(FilterData, Dependencies, State),
    Candidates = [{QId2#qid.no,Pattern,[Filter],F} || 
                     {QId,[QId2]} <- Dependencies,
                     {GQId,{gen,Pattern,_}} <- GeneratorData,
                     GQId =:= QId2,
                     {FQId,{fil,F}}=Filter <- Filters, % guard filters only
                     FQId =:= QId] 
               ++ [{GId#qid.no,Pattern,[],{atom,0,true}} || 
                      {GId,{gen,Pattern,_}} <- GeneratorData,
                      lists:member(GId, NoFilterGIds)],
    E = {nil, 0},
    GF = [{{GNum,Pattern},Filter} || 
             {GNum,Pattern,Filter,F} <- Candidates,
             no =/= try_ms(E, Pattern, F, State)],
    GFF = sofs:relation_to_family(sofs:relation(GF, 
                                                [{gnum_pattern,[filter]}])),
    GFFL = sofs:to_external(sofs:family_union(GFF)),
    try
        [{{GNum,Pattern}, GFilterData}] = GFFL,
        true = length(GFilterData) =:= length(FilterData),
        [_] = GeneratorData,
        AbstrMS = gen_ms(Template, Pattern, GFilterData, State),
        %% There is one generator and every filter uses some of the
        %% variables introduced by the generator. The whole qlc
        %% expressione can be replaced by a match specification.
        [{GNum, AbstrMS, all}]
    catch _:_ ->
        {TemplVar, _} = anon_var({var,0,'_'}, 0),
        [one_gen_match_spec(GNum, Pattern, GFilterData, State, TemplVar) ||
            {{GNum,Pattern},GFilterData} <- GFFL]
    end.

one_gen_match_spec(GNum, Pattern0, GFilterData, State, TemplVar) ->
    {E, Pattern} = pattern_as_template(Pattern0, TemplVar),
    AbstrMS = gen_ms(E, Pattern, GFilterData, State),
    {GNum, AbstrMS, [FId#qid.no || {FId,_} <- GFilterData]}.

gen_ms(E, Pattern, GFilterData, State) ->
    {ok, MS, AMS} = try_ms(E, Pattern, filters_as_one(GFilterData), State),
    case MS of
        [{'$1',[true],['$1']}] ->
            {atom, 0, no_match_spec};
        _ ->
            AMS
    end.

%% -> {Template, Pattern'}
%% The pattern is accepted by ets:fun2ms/1, that is, =/2 can only
%% occur at top level. Introduce or reuse a top-level variable as
%% template
pattern_as_template({var,_,'_'}, TemplVar) ->
    {TemplVar, TemplVar};
pattern_as_template({var,_,_}=V, _TemplVar) ->
    {V, V};
pattern_as_template({match,L,E,{var,_,'_'}}, TemplVar) ->
    {TemplVar, {match,L,E,TemplVar}};
pattern_as_template({match,L,{var,_,'_'},E}, TemplVar) ->
    {TemplVar, {match,L,E,TemplVar}};
pattern_as_template({match,_,_E,{var,_,_}=V}=P, _TemplVar) ->
    {V, P};
pattern_as_template({match,_,{var,_,_}=V,_E}=P, _TemplVar) ->
    {V, P};
pattern_as_template(E, TemplVar) ->
    L = 0,
    {TemplVar, {match, L, E, TemplVar}}.

%% Tries to find columns which are matched against constant values or
%% other columns. To that end unification is used. A frame is a list
%% of bindings created by unification.
%% Also tries to find the number of columns of patterns.
%% Note that the template is handled more or less as a pattern.
%% -> {ColumnConstants, PatternSizes, ExtraConstants}
%% ColumnConstants = [{Column,[Constant],[FilterNo]}]
%% PatternSizes = [{QualifierNumber,NumberOfColumns}]
%% Column = {QualifierNumber,ColumnNumber}}
%% FilterNo is a filter that can be skipped at runtime provided constants
%% are looked up.
%% ExtraConstants = [{GeneratorNumber,[{ColumnNumber,AbstractConstants}]}]
%% For every generator such that the unification binds value(s) to
%% some column(s), extra constants are returned. These constants are
%% the results of the unification, and do not occur in the pattern of
%% the generator.
constants_and_sizes(Qualifiers0, E, Dependencies, AllIVs, State) ->
    TemplateAsPattern = template_as_pattern(E),
    Qualifiers = [TemplateAsPattern | Qualifiers0],
    {FilterData, GeneratorData} = qual_data(Qualifiers),
    {Filter, Anon1, Imported} = 
        filter_info(FilterData, AllIVs, Dependencies, State),
    BindFun = fun(Value, Op) -> is_bindable(Value, Op, Imported) end,
    {PatternFrame, PatternVars} = 
        pattern_frame(GeneratorData, BindFun, Anon1, State),
    SkipFun = fun(Fs) -> Fs end,
    Fs = filter(Filter, PatternFrame, BindFun, SkipFun, State),

    Selector = fun(Value) -> is_const(Value, Imported) end,
    ColumnConstants0 = [frames_to_columns(Fs, PV, QId, Selector) || 
                           {QId,PV} <- PatternVars],
    ColumnConstants1 = flatten(ColumnConstants0),

    PatternConstants = 
        flatten([frames_to_columns([PatternFrame], PV, QId, Selector) || 
                    {QId,PV} <- PatternVars]),
    ExtraConstants = 
      family_list([{GId, {Col,Vals}} ||
                      {{GId,Col},Vals} <- ColumnConstants1--PatternConstants,
                      GId =/= ?TNO]),

    ColumnConstants = lu_skip(ColumnConstants1, FilterData, BindFun, Selector,
                              PatternFrame, PatternVars, Dependencies, State),
    PatternSizes = [{QId#qid.no, Size} || 
                       {QId,PV} <- PatternVars,
                       undefined =/= (Size = pattern_size(Fs, PV))],
    {ColumnConstants, PatternSizes, ExtraConstants}.

%% Augment ColConstants with filters that do not need to be run
%% provided that constants are looked up.
lu_skip(ColConstants, FilterData, BindFun, Selector, PatternFrame, 
        PatternVars, Dependencies, State) ->
    %% If there is a test that does not compare or match, then the
    %% filter cannot be skipped.
    FailSelector = fun(_Value) -> true end,
    %% In runtime, constants are looked up and matched against a pattern 
    %% (the pattern acts like a filter), then the filters are run.
    PatternColumns = 
        flatten([frames_to_columns([PatternFrame], PV, QId, FailSelector) ||
                    {QId,PV} <- PatternVars]),
    %% Note: ColFil can contain filters for columns that cannot be
    %% looked up. Such (possibly bogus) elements are however not used.
    %% Note: one filter at a time is tested; only the pattern is
    %% assumed to have been run when the filter is run. Sometimes it
    %% would be advantageously to assume some filter(s) occurring
    %% before the filter had been run as well 
    %% (an example: {{X,Y}} <- LE, X =:= 1, Y =:= a).
    ColFil = [{Column, FId#qid.no} ||
                 {FId,{fil,Fil}} <- 
                     filter_list(FilterData, Dependencies, State),
                 [] =/= (SFs = safe_filter(set_line(Fil, 0), 
                                           PatternFrame, BindFun, State)),
                 {GId,PV} <- PatternVars,
                 GId#qid.lcid =:= FId#qid.lcid,
                 [] =/= (F = frames_to_columns(SFs, PV, GId, Selector)),
                 %% The filter must not test more than one column (unless the
                 %% pattern has already done the test):
                 length(D = F -- PatternColumns) =:= 1,
                 Frame <- SFs,
                 begin
                     %% The column is compared/matched to a constant.
                     %% If there are no more comparisons/matches then
                     %% the filter can be replaced by the lookup of
                     %% the constant.
                     [{{_,Col} = Column, Constants}] = D,
                     Value = column_i(Frame, PV, Col),
                     PVar = {var, 0, PV},
                     Call = {call,0,{atom,0,element},[{integer,0,Col},PVar]},
                     {NV, F1} = element_calls(Call, PatternFrame, BindFun),
                     F2 = unify('=:=', NV, Value, F1, BindFun),
                     %% F2: the pattern has been matched and the
                     %% constant has been looked up. If Frame has no
                     %% more bindings than F2 (modulo unique
                     %% variables), then the filter can be skipped. 
                     %% 
                     %% Under rare circumstances (for instance: 
                     %% "X =:= 1, X =:= U", U imported; only 1 is looked up),
                     %% not all constants mentioned in a filter are looked up.
                     %% The filter can only be skipped if all constants
                     %% are looked up.
                     LookedUpConstants = 
                         case lists:keysearch(Column, 1, ColConstants) of
                             false -> [];
                             {value, {Column,LUCs}} -> LUCs
                         end,
                     bindings_is_subset(Frame, F2)
                     and (Constants -- LookedUpConstants =:= [])
                 end],
    ColFils = family_list(ColFil),
    %% The atom 'all' means that all filters are covered by the lookup.
    %% It does not imply that there is only one generator as is the case
    %% for match specifications (see match_spec_quals above).
    [{Col,Constants,case keysearch(Col, 1, ColFils) of
                        {value, {Col, FilL}} -> 
                            Tag = if
                                      length(FilterData) =:= length(FilL) ->
                                          all;
                                      true ->
                                          some
                                  end,
                            {Tag, FilL};
                        false -> 
                            {some,[]}
                    end} || {Col,Constants} <- ColConstants].

equal_columns(Qualifiers, AllIVs, Dependencies, State) ->
    Cs = equal_columns2(Qualifiers, AllIVs, Dependencies, State),
    join_gens(Cs).

eq_columns(Qualifiers, AllIVs, Dependencies, State) ->
    Cs = eq_columns2(Qualifiers, AllIVs, Dependencies, State),
    join_gens(Cs).

%% Group columns of the same generator together.
%% -> {TwoGen, ManyGens}
join_gens(Cs0) ->
    Cs = [family_list(C) || C <- Cs0],
    {lists:filter(fun(C) -> length(C) =:= 2 end, Cs),
     lists:filter(fun(C) -> length(C) > 2 end, Cs)}.

%% Tries to find columns (possibly in the same table) that are
%% matched (=:=/2) or compared (==/2). Unification again.
%% -> [[{QualifierNumber,ColumnNumber}]] % Eq.classes.

equal_columns2(Qualifiers, AllIVs, Dependencies, State) ->
    BindFun = fun(Imp) -> fun(V, Op) -> is_no_const(V, Op, Imp) end end,
    join_info(Qualifiers, AllIVs, Dependencies, BindFun, State).

%% Tries to find columns (possibly in the same table) that are matched
%% (=:=/2).
%% -> [[{QualifierNumber,ColumnNumber}]] % Eq.classes.

eq_columns2(Qualifiers, AllIVs, Dependencies, State) ->
    BindFun = fun(Imp) -> fun(V, Op) -> is_match_no_const(V, Op, Imp) end end,
    join_info(Qualifiers, AllIVs, Dependencies, BindFun, State).

join_info(Qualifiers, AllIVs, Dependencies, BindFun0, State) ->
    {FilterData, GeneratorData} = qual_data(Qualifiers),
    {Filter, Anon1, Imported} = 
        filter_info(FilterData, AllIVs, Dependencies, State),
    BindFun = BindFun0(Imported),
    {PatternFrame, PatternVars} = 
        pattern_frame(GeneratorData, BindFun, Anon1, State),
    SkipFun = fun(Fs) -> Fs end,
    Fs = filter(Filter, PatternFrame, BindFun, SkipFun, State),
    Selector = fun(Value) -> not is_const(Value, Imported) end,
    join_classes(fun(PV, QId) -> frames_to_columns(Fs, PV, QId, Selector) 
                 end, PatternVars).

join_classes(FramesFun, PatternVars) ->
    Cols0 = [FramesFun(PV, QId) || {QId,PV} <- PatternVars],
    ColVar = sofs:relation(append(Cols0)),
    Cols = sofs:partition(2, ColVar),
    [[C || {C,_} <- Cs] || Cs <- sofs:to_external(Cols), length(Cs) > 1].

filter_info(FilterData, AllIVs, Dependencies, State) ->
    FilterList = filter_list(FilterData, Dependencies, State),
    Filter0 = set_line(filters_as_one(FilterList), 0),
    Anon0 = 0,
    {Filter, Anon1} = anon_var(Filter0, Anon0),
    Imported = ordsets:subtract(vars(Filter), % anonymous too
                                sort(AllIVs)), 
    {Filter, Anon1, Imported}.

%% Selects the guard filters. Other filters than guard filters are
%% ignored when trying to find constants and join columns. Note: there
%% must not occur any non-guard filter between a guard filter and the
%% generator(s) the guard filter depends on. The reason is that such a
%% filter could fail for some object(s) excluded by lookup or join. If
%% the failing filter is placed _after_ the guard filter, the failing
%% objects have already been filtered out by the guard filter.
filter_list(FilterData, Dependencies, State) ->
    RecordDefs = State#state.records,
    {GuardFilters, OtherFilters} =
        lists:partition(fun({_QId,{fil,F}}) ->
                                erl_lint:is_guard_test(F, RecordDefs)
                        end, FilterData),
    [Filter || Filter={Id,_} <- GuardFilters,
               [] =:= [GId || {Id2, GIds} <- Dependencies,
                              Id2 =:= Id,
                              GId <- GIds,
                              {OId, _} <- OtherFilters,
                              OId < Id,    % OId comes before Id
                              OId > GId]]. % OId comes after GId

pattern_frame(GeneratorData, BindFun, Anon1, State) ->
    Frame0 = [],
    {PatternFrame, _Anon2, PatternVars} =
        lists:foldl(fun({QId,{gen,Pattern,_}}, {F0,An0,PVs}) ->
                            {F1, An1, PV} = 
                                pattern(Pattern, An0, F0, BindFun, State),
                            {F1, An1, [{QId,PV} | PVs]}
                    end, {Frame0, Anon1, []}, GeneratorData),
    {PatternFrame, PatternVars}.
              
is_match_no_const(Value, Op, Imported) ->
    (Op =/= '==') andalso is_no_const(Value, Op, Imported).

is_no_const(Value, Op, Imported) ->
    is_bindable(Value, Op, Imported) andalso not is_const(Value, Imported).

%% Tuple tails are variables, never constants.
is_const(Value, Imported) ->
    %% is_bindable() has checked that E is normalisable. 
    [] =:= ordsets:to_list(ordsets:subtract(vars(Value), Imported)).

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

is_bindable(Value, Op, Imp) ->
    case normalise(Value) of
        {ok, NValue} when Op =:= '==' ->
            case {ordsets:to_list(ordsets:intersection(vars(Value), Imp)), 
                  has_integer(NValue)} of
                {[], false} ->
                    true;
                _ ->
                    false
            end;
        {ok, _} when Op =:= '=:=' ->
            true;
        not_ok ->
            false
    end.

pattern(P0, AnonI, Frame0, BindFun, State) ->
    P1 = try 
             expand_pattern_records(P0, State)
         catch _:_ -> P0 % template, records already expanded
         end,
    %% Makes test for equality simple:
    P2 = set_line(P1, 0),
    {P3, AnonN} = anon_var(P2, AnonI),
    {P4, F1} = match_in_pattern(tuple2cons(P3), Frame0, BindFun),
    {P, F2} = element_calls(P4, F1, BindFun), % kludge for templates
    {var, _, PatternVar} = UniqueVar = unique_var(),
    F = unify('=:=', UniqueVar, P, F2, BindFun),
    {F, AnonN, PatternVar}.

match_in_pattern({match, _, E10, E20}, F0, BF) ->
    {E1, F1} = match_in_pattern(E10, F0, BF),
    {E2, F} = match_in_pattern(E20, F1, BF),
    %% This is for join: chosing a constant could "hide" a variable.
    E = case BF(E1, '=:=') of
            true -> E1;
            false -> E2
        end,
    {E, unify('=:=', E1, E2, F, BF)};
match_in_pattern(T, F0, BF) when is_tuple(T) ->
    {L, F} = match_in_pattern(tuple_to_list(T), F0, BF),
    {list_to_tuple(L), F};
match_in_pattern([E0 | Es0], F0, BF) ->
    {E, F1} = match_in_pattern(E0, F0, BF),
    {Es, F} = match_in_pattern(Es0, F1, BF),
    {[E | Es], F};
match_in_pattern(E, F, _BF) ->
    {E, F}.

-define(ANON_VAR(N), N).

anon_var(E, AnonI) ->
    var_mapfold(fun({var, L, '_'}, N) ->
                        {{var, L, ?ANON_VAR(N)}, N+1};
                   (Var, N) -> {Var, N}
                end, AnonI, E).

set_line(T, L) ->
    map_lines(fun(_L) -> L end, T).

-record(fstate, {state, bind_fun, skip_fun}).

filter(_E, failed, _BF, _SF, _State) ->
    [];
filter(E0, Frame0, BF, SF, State) ->
    E = pre_expand(E0),
    FState = #fstate{state = State, bind_fun = BF, skip_fun = SF},
    filter1(E, [Frame0], FState).

%% One frame for each path through the and/or formula.
%%
%% "A xor B" is equal to "(A and not B) or (not A and B)". 
%% Ignoring "not B" and "not A" this is the same as "A or B"; 
%% "xor" can be handled just as "or".
%% 
%% One must handle filters with care, both when joining and when
%% looking up values. The reference is a nested loop: if the filter
%% fails for some combination of values, it must fail also when
%% looking up values or joining. In other words, the excluded
%% combinations of values must not evaluate to anything but 'false'.
%% Filters looking like guards can fail since for such filter the so
%% called guard semantics ensures that the value is 'false' if it is
%% not 'true'. This behavior was inherited from the ordinary list
%% comprehension, where it has been considered a bug kept for backward
%% compatibility. Now it has become a part of QLC, and hard to change
%% (at least in QLC).
%%
%% A special case is =/2. If there is a chance that the =/2 fails
%% (badmatch) for some combination of values, that combination cannot
%% be excluded. If the variable is bound once only, it is OK, but not
%% twice (or more). The current implementation does not handle =/2 at
%% all (except in generator patterns).

filter1({op, _, Op, L0, R0}, Fs, FS) when Op =:= '=:='; Op =:= '==' ->
    #fstate{state = S, bind_fun = BF} = FS,
    %% In the transformed code there are no records in lookup values
    %% because records are expanded away in prep_expr.
    flatmap(fun(F0) ->
                    {L, F1} = prep_expr(L0, F0, S, BF),
                    {R, F2} = prep_expr(R0, F1, S, BF),
                    case unify(Op, L, R, F2, BF) of
                        failed -> [];
                        F -> [F]
                    end
            end, Fs);
filter1({op, _, Op, L, R}, Fs, FS) when Op =:= 'and'; Op =:= 'andalso' ->
    filter1(R, filter1(L, Fs, FS), FS);
filter1({op, _, Op, L, R}, Fs, FS) when Op =:= 'or'; 
                                        Op =:= 'orelse';
                                        Op =:= 'xor' ->
    filter1(L, Fs, FS) ++ filter1(R, Fs, FS);
filter1({atom,_,Atom}, _Fs, _FS) when Atom =/= true ->
    [];
filter1({call,L,{remote,_,{atom,_,erlang},{atom,_,is_record}},[T,R]},
        Fs, FS) ->
    filter1({op,L,'=:=',{call,L,{remote,L,{atom,L,erlang},{atom,L,element}},
                         [{integer,L,1},T]},R},
            Fs, FS);
%% erlang:is_record/3 (the size information is ignored):
filter1({call,L,{remote,L1,{atom,_,erlang}=M,{atom,L2,is_record}},[T,R,_Sz]},
        Fs, FS) ->
    filter1({call,L,{remote,L1,M,{atom,L2,is_record}},[T,R]}, Fs, FS);
filter1(_E, Fs, FS) ->
    (FS#fstate.skip_fun)(Fs).

%% filter() tries to extract as much information about constant
%% columns as possible. It ignores those parts of the filter that are
%% uninteresting. safe_filter() on the other hand ensures that the
%% bindings returned capture _all_ aspects of the filter.
safe_filter(_E, failed, _BF, _State) ->
    [];
safe_filter(E0, Frame0, BF, State) ->
    E = pre_expand(E0),
    FailFun = fun(_Fs) -> [] end,
    FState = #fstate{state = State, bind_fun = BF, skip_fun = FailFun},
    safe_filter1(E, [Frame0], FState).

safe_filter1({op, _, Op, L0, R0}, Fs, FS) when Op =:= '=:='; Op =:= '==' ->
    #fstate{state = S, bind_fun = BF} = FS,
    flatmap(fun(F0) ->
                    {L, F1} = prep_expr(L0, F0, S, BF),
                    {R, F2} = prep_expr(R0, F1, S, BF),
                    case safe_unify(Op, L, R, F2, BF) of
                        failed -> [];
                        F -> [F]
                    end
            end, Fs);
safe_filter1({op, _, Op, L, R}, Fs, FS) when Op =:= 'and'; Op =:= 'andalso' ->
    safe_filter1(R, safe_filter1(L, Fs, FS), FS);
safe_filter1({op, _, Op, L, R}, Fs, FS) when Op =:= 'or'; Op =:= 'orelse' ->
    safe_filter1(L, Fs, FS) ++ safe_filter1(R, Fs, FS);
safe_filter1({atom,_,Atom}, _Fs, _FS) when Atom =/= true ->
    [];
safe_filter1(_E, Fs, FS) ->
    (FS#fstate.skip_fun)(Fs).

%% Substitutions: 
%% M:F() for {M,F}(); erlang:F() for F(); is_record() for record().
pre_expand({call,L1,{atom,L2,record},As}) ->
    pre_expand({call,L1,{atom,L2,is_record},As});
pre_expand({call,L,{atom,_,_}=F,As}) ->
    pre_expand({call,L,{remote,L,{atom,L,erlang},F},As});
pre_expand({call,L,{tuple,_,[M,F]},As}) ->
    pre_expand({call,L,{remote,L,M,F},As});
pre_expand(T) when is_tuple(T) ->
    list_to_tuple(pre_expand(tuple_to_list(T)));
pre_expand([E | Es]) ->
    [pre_expand(E) | pre_expand(Es)];
pre_expand(T) ->
    T.

column_i(Frame, PatternVar, I) ->
    {cons_tuple, Cs} = deref({var, 0, PatternVar}, Frame),
    column_i_2(Cs, 1, I).

column_i_2({cons,_,V,_}, I, I) ->
    V;
column_i_2({cons,_,_,E}, I, N) ->
    column_i_2(E, I+1, N).

pattern_size(Fs, PatternVar) ->
    Szs = map(fun(F) ->
                      case deref({var, 0, PatternVar}, F) of
                          {cons_tuple, Cs} -> pattern_sz(Cs, 0);
                          _ -> undefined
                      end
              end, Fs),
    case lists:usort(Szs) of
        [Sz] when Sz >= 0 -> Sz;
        _  -> undefined
    end.

pattern_sz({cons,_,_C,E}, Col) ->
    pattern_sz(E, Col+1);
pattern_sz({nil,_}, Sz) ->
    Sz;
pattern_sz(_, _Sz) ->
    undefined.

%% -> [{{QualifierNumber,ColumnNumber}, [Value]}]
frames_to_columns(Fs, PatternVar, PatternId, Selector) ->
    F = fun({cons_tuple, Cs}) -> 
                sel_columns(Cs, 1, PatternId, Selector);
           (_) -> 
                []
        end,
    all_frames(Fs, PatternVar, F).

sel_columns({cons,_,C,E}, Col, PId, Selector) ->
    case Selector(C) of
        true -> 
            V = {{PId#qid.no,Col},cons2tuple(C)},
            [V | sel_columns(E, Col+1, PId, Selector)];
        false ->
            sel_columns(E, Col+1, PId, Selector)
    end;
sel_columns(_, _Col, _PId, _Selector) ->
    [].

all_frames([], _PatternVar, _DerefFun) ->
    [];
all_frames(Fs, PatternVar, DerefFun) ->
    Rs = map(fun(F) ->
                     Deref = deref({var, 0, PatternVar}, F),
                     RL = DerefFun(Deref),
                     sofs:relation(RL) % possibly empty
             end, Fs),
    Ss = sofs:from_sets(Rs),
    %% D: columns occurring in every frame (path).
    D = sofs:intersection(sofs:projection(fun(S) -> sofs:projection(1, S) end,
                                          Ss)),
    Cs = sofs:restriction(sofs:relation_to_family(sofs:union(Ss)), D),
    sofs:to_external(Cs).

prep_expr(E, F, S, BF) ->
    element_calls(tuple2cons(expand_expr_records(E, S)), F, BF).

%% cons_tuple is used for representing {V1, ..., Vi | TupleTail}.
%%
%% Tests like "element(2, X) =:= a" are represented by "tuple tails":
%% {_, a | _}. The tail may be unified later, when more information
%% about the size of the tuple is known.
element_calls({call,_,{remote,_,{atom,_,erlang},{atom,_,element}},
               [{integer,_,I},Term0]}, F0, BF) when I > 0 ->
    VarI = unique_var(),
    TupleTail = unique_var(),
    Tuple = element_tuple(I, [VarI | TupleTail]),
    {Term, F} = element_calls(Term0, F0, BF),    
    {VarI, unify('=:=', Tuple, Term, F, BF)};
element_calls({call,L1,{atom,_,element}=E,As}, F0, BF) ->
    %% erl_expand_records should add "erlang:"...
    element_calls({call,L1,{remote,L1,{atom,L1,erlang},E}, As}, F0, BF);
element_calls(T, F0, BF) when is_tuple(T) ->
    {L, F} = element_calls(tuple_to_list(T), F0, BF),
    {list_to_tuple(L), F};
element_calls([E0 | Es0], F0, BF) ->
    {E, F1} = element_calls(E0, F0, BF),
    {Es, F} = element_calls(Es0, F1, BF),
    {[E | Es], F};
element_calls(E, F, _BF) ->
    {E, F}.

element_tuple(1, Es) ->
    {cons_tuple, list2cons(Es)};
element_tuple(I, Es) ->
    element_tuple(I-1, [unique_var() | Es]).

unique_var() ->
    {var, 0, make_ref()}.

is_unique_var({var, _L, V}) ->
    is_reference(V).

expand_pattern_records(P, State) ->
    E = {'case',0,{atom,0,true},[{clause,0,[P],[],[{atom,0,true}]}]},
    {'case',_,_,[{clause,0,[NP],_,_}]} = expand_expr_records(E, State),
    NP.

expand_expr_records(E, State) ->
    RecordDefs = State#state.records,
    Forms = RecordDefs ++ [{function,1,foo,0,[{clause,1,[],[],[pe(E)]}]}],
    [{function,_,foo,0,[{clause,_,[],[],[NE]}]}] = 
        erl_expand_records:module(Forms, [no_strict_record_tests]),
    NE.

%% Partial evaluation.
pe({op,Line,Op,A}) ->
    erl_eval:partial_eval({op,Line,Op,pe(A)});
pe({op,Line,Op,L,R}) ->
    erl_eval:partial_eval({op,Line,Op,pe(L),pe(R)});
pe(T) when is_tuple(T) ->
    list_to_tuple(pe(tuple_to_list(T)));
pe([E | Es]) ->
    [pe(E) | pe(Es)];
pe(E) ->
    E.

unify(Op, E1, E2, F, BF) ->
    unify(Op, E1, E2, F, BF, false).

safe_unify(Op, E1, E2, F, BF) ->
    unify(Op, E1, E2, F, BF, true).

unify(_Op, _E1, _E2, failed, _BF, _Safe) -> % contradiction
    failed;
unify(_Op, E, E, F, _BF, _Safe) ->
    F;
unify(Op, {var, _, _}=Var, E2, F, BF, Safe) ->
    extend_frame(Op, Var, E2, F, BF, Safe);
unify(Op, E1, {var, _, _}=Var, F, BF, Safe) ->
    extend_frame(Op, Var, E1, F, BF, Safe);
unify(Op, {cons_tuple, Es1}, {cons_tuple, Es2}, F, BF, Safe) ->
    unify(Op, Es1, Es2, F, BF, Safe);
unify(Op, {cons, _, L1, R1}, {cons, _, L2, R2}, F, BF, Safe) ->
    unify(Op, R1, R2, unify(Op, L1, L2, F, BF, Safe), BF, Safe);
unify(Op, E1, E2, F, _BF, Safe) ->
    %% This clause could just return F.
    try
      {ok, C1} = normalise(E1),
      {ok, C2} = normalise(E2),
      if 
          Op =:= '=:=', C1 =:= C2 ->
              F;
          Op =:= '==', C1 == C2 ->
              F;
          true ->
              failed
      end 
    catch error:_ when Safe -> failed;
          error:_ when not Safe -> F   % ignored
    end.
%% Binaries are not handled at all.

-record(bind, {var, value}).

extend_frame(Op, Var, Value, F, BF, Safe) ->
    case binding(Var, F) of
        #bind{var = Var, value = VarValue} ->
            unify(Op, VarValue, Value, F, BF, Safe);
        false ->
            case Value of
                {var, _, _} ->
                    case binding(Value, F) of
                        #bind{var = Value, value = ValueValue} ->
                            unify(Op, Var, ValueValue, F, BF, Safe);
                        false ->
                            add_binding(Op, Var, Value, F, BF)
                    end;
                _ ->
                    add_binding(Op, Var, Value, F, BF)
            end
    end.
    
add_binding(Op, Var, Value, F, BF) ->
    case BF(Value, Op) of
        true -> 
            add_binding(Var, Value, F);
        false ->
            F
    end.

add_binding(Var, Value, F) ->
    case {occurs(Var, Value, F), Value} of
        {true, _} ->
            failed;
        {false, {var, _, Ref}} when is_reference(Ref) ->
            %% Push imported variables to the end of the binding chain
            %% in order to make is_const/1 work.
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

%% Returns true if all bindings in F1 also occur in F2.
%% All unique variables are considered equal after deref.
bindings_is_subset(F1, F2) ->
    lists:all(fun(#bind{var = V}) ->
                      is_unique_var(V) 
                      orelse (defef_ss(V, F1) =:= defef_ss(V, F2))
              end, F1).

defef_ss(E, F) ->
    var_map(fun(V) ->
                    case is_unique_var(V) of
                        true -> unique_var;
                        false -> V
                    end
            end, deref(E, F)).

%% Recognizes all QLCs on the form [T || P <- LE, F] such that
%% ets:fun2ms(fun(P) when F -> T end) is a match spec. This is OK with
%% the guard semantics implemented in filter/_ below. If one chooses
%% not to have guard semantics, affected filters will have to be
%% recognized and excluded here as well.
try_ms(E, P, Fltr, State) ->
    L = 1,
    Fun =  {'fun',L,{clauses,[{clause,L,[P],[[Fltr]],[E]}]}},
    Expr = {call,L,{remote,L,{atom,L,ets},{atom,L,fun2ms}},[Fun]},
    Form0 = {function,L,foo,0,[{clause,L,[],[],[Expr]}]},
    Form = restore_line_numbers(Form0),
    X = ms_transform:parse_transform(State#state.records ++ [Form], []),
    case catch 
        begin
            {function,L,foo,0,[{clause,L,[],[],[MS0]}]} = lists:last(X),
            MS = erl_parse:normalise(var2const(MS0)),
            XMS = ets:match_spec_compile(MS),
            true = is_binary(XMS),
            {ok, MS, MS0} 
        end of
        {'EXIT', _Reason} ->
            no;
        Reply ->
            Reply
    end.

filters_as_one([]) ->
    {atom, 0, true};
filters_as_one(FilterData) ->
    [{_,{fil,Filter1}} | Filters] = lists:reverse(FilterData),
    lists:foldr(fun({_QId,{fil,Filter}}, AbstF) ->
                        {op,0,'andalso',Filter,AbstF}
                end, Filter1, Filters).

qual_data(Qualifiers) ->
    F = fun(T) -> 
                [{QId,Q} || {QId,_,_,Q} <- Qualifiers, element(1,Q) =:= T]
        end,
    {F(fil), F(gen)}.

set_field(Pos, Fs, Data) ->
    lists:sublist(Fs, Pos-1) ++ [Data] ++ lists:nthtail(Pos, Fs).

qdata([{#qid{no = QIdNo},{_QIVs,{{gen,_P,LE,_GV},GoI,SI}}} | QCs], L) ->
    Init = case LE of 
               {join, Op, Q1, Q2, H1, H2, Cs1_0, Cs2_0} ->
                   Cs1 = qcon(Cs1_0),
                   Cs2 = qcon(Cs2_0),
                   Compat = {nil,L}, % meant for redundant match spec
                   CF = closure({tuple,L,[Cs1,Cs2,Compat]}, L),
                   {tuple,L,[?A(join),?A(Op),?I(Q1),?I(Q2),H1,H2,CF]};
               _ ->
                   closure(LE, L)
           end,
    %% Create qual_data (see qlc.erl):
    {cons,L,{tuple,L,[?I(QIdNo),?I(GoI),?I(SI),{tuple,L,[?A(gen),Init]}]},
     qdata(QCs, L)};
qdata([{#qid{no = QIdNo},{_QIVs,{{fil,_F},GoI,SI}}} | QCs], L) ->
    %% Create qual_data (see qlc.erl):
    {cons,L,{tuple,L,[?I(QIdNo),?I(GoI),?I(SI),?A(fil)]},qdata(QCs, L)};
qdata([], L) ->
    {nil,L}.

qcon(Cs) ->
    abstr([{C,[erl_parse:normalise(V) || V <- Vs]} || {C,Vs} <- Cs], 0).

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

clauses([{QId,{QIVs,{QualData,GoI,S}}} | QCs], RL, Fun, Go, NGV, E, IVs,St) ->
    ?DEBUG("QIVs = ~p~n", [QIVs]),
    ?DEBUG("IVs = ~p~n", [IVs]),
    ?DEBUG("GoI = ~p, S = ~p~n", [GoI, S]),
    L = no_compiler_warning(get_lcid_line(QId#qid.lcid)),
    Cs = case QualData of
             {gen,P,_LE,GV} ->
                 generator(S, QIVs, P, GV, NGV, E, IVs, RL, Fun, Go,GoI,L,St);
             {fil,F} ->
                 filter(F, L, QIVs, S, RL, Fun, Go, GoI, IVs, St)
         end,
    Cs ++ clauses(QCs, RL, Fun, Go, NGV, E, IVs, St);
clauses([], _RL, _Fun, _Go, _NGV, _IVs, _E, _St) ->
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

generator(S, QIVs, P, GV, NGV, E, IVs, RL, Fun, Go, GoI, L, State) ->
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
    CsF = generator_cont(P, GV, NGV, E, As, AsM, AsC, AsD, Fun, L, State),
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

%% The clause 'CE' was added in R11B. The version of the generated was
%% however not incremented.
generator_cont(P, GV, NGV, E, As0, AsM, AsC, AsD, Fun, L, State) ->
    As = pack_args(As0, L, State),
    CF1 = ?ABST_MORE(P, ?V(NGV)),
    CF2 = ?ABST_MORE(?V('_'), ?V(NGV)),
    CF3 = ?ABST_NO_MORE,
    CF4 = ?V(E),
    CM = {clause,L,[CF1],[],[{call,L,?V(Fun),AsM}]},
    CC = {clause,L,[CF2],[],[{call,L,?V(Fun),AsC}]},
    CD = {clause,L,[CF3],[],[{call,L,?V(Fun),AsD}]},
    CE = {clause,L,[CF4],[],[CF4]},
    Cls = [CM, CC, CD, CE],
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

list2op([E], _Op) ->
    E;
list2op([E | Es], Op) ->
    {op,0,Op,E,list2op(Es, Op)}.

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
    erase(?QLC_FILE),
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
qlcmf({attribute,_L,file,{File,_Line}}=Attr, _F, _Imp, A, No) ->
    put(?QLC_FILE, File),
    {Attr, A, No};
qlcmf(T, F, Imp, A0, No0) when is_tuple(T) ->
    {TL, A, No} = qlcmf(tuple_to_list(T), F, Imp, A0, No0),
    {list_to_tuple(TL), A, No};
qlcmf(T, _F, _Imp, A, No) ->
    {T, A, No}.

vars(E) ->
    var_ufold(fun({var,_L,V}) -> V end, E).

occ_vars(E) ->
    var_fold(fun({var,_L,V}) -> V end, [], E).

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
        case keysearch(V, 1, PVs0) of
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
    sgn(Line) * ((No bsl ?MAX_NUM_OF_LINES) + sgn(Line) * Line).

is_lcid(Id) ->
    is_integer(Id) andalso (abs(Id) > (1 bsl ?MAX_NUM_OF_LINES)).

get_lcid_no(Id) ->
    abs(Id) bsr ?MAX_NUM_OF_LINES.

get_lcid_line(Id) ->
    sgn(Id) * (abs(Id) band ((1 bsl ?MAX_NUM_OF_LINES) - 1)).

sgn(X) when X >= 0 ->
    1;
sgn(X) when X < 0 ->
    -1.

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

family_list(L) ->
    sofs:to_external(family(L)).

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

