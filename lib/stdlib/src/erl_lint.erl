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
%% Do necessary checking of Erlang code.

%% N.B. All the code necessary for checking structs (tagged tuples) is
%% here. Just comment out the lines in pattern/2, gexpr/3 and expr/3.

-module(erl_lint).

-export([module/1,module/2,module/3,format_error/1]).
-export([exprs/2,used_vars/2]). % Used from erl_eval.erl.
-export([is_pattern_expr/1,is_guard_test/1,is_guard_test/2]).
-export([is_guard_expr/1]).
-export([bool_option/4,value_option/3,value_option/7]).

-import(lists, [member/2,map/2,foldl/3,foldr/3,mapfoldl/3,all/2]).

%% bool_option(OnOpt, OffOpt, Default, Options) -> true | false.
%% value_option(Flag, Default, Options) -> Value.
%% value_option(Flag, Default, OnOpt, OnVal, OffOpt, OffVal, Options) ->
%%              Value.
%%  The option handling functions.

bool_option(On, Off, Default, Opts) ->
    foldl(fun (Opt, _Def) when Opt == On -> true;
              (Opt, _Def) when Opt == Off -> false;
              (_Opt, Def) -> Def
          end, Default, Opts).

value_option(Flag, Default, Opts) ->
    foldl(fun ({Opt,Val}, _Def) when Opt == Flag -> Val;
              (_Opt, Def) -> Def
          end, Default, Opts).

value_option(Flag, Default, On, OnVal, Off, OffVal, Opts) ->
    foldl(fun ({Opt,Val}, _Def) when Opt == Flag -> Val;
              (Opt, _Def) when Opt == On -> OnVal;
              (Opt, _Def) when Opt == Off -> OffVal;
              (_Opt, Def) -> Def
          end, Default, Opts).

%% The error and warning info structures, {Line,Module,Descriptor}, are
%% kept in reverse order in their seperate fields in the lint state record.
%% When a new file is entered, marked by the file attribute then a
%% {file,FileName} pair is pushed on each list. At the end of the run these
%% lists are packed, and reversed, into a list of {FileName,ErrorDescList}
%% pairs which are returned.

-include_lib("stdlib/include/erl_bits.hrl").

%%-define(DEBUGF(X,Y), io:format(X, Y)).
-define(DEBUGF(X,Y), void).

%% Define the lint state record.
%% 'called' and 'exports' contain {Line, {Function, Arity}},
%% the other function collections contain {Function, Arity}.
%% 'called' is a list, not an ordset.
-record(lint, {state=start,                     %start | attribute | function
               module=[],                       %Module
               package="",                      %Module package
               behaviour=[],                    %Behaviour
               exports=[],                      %Exports
               imports=[],                      %Imports
               mod_imports=dict:new(),          %Module Imports
               compile=[],                      %Compile flags
               records=orddict:new(),           %Record definitions
               defined=[],                      %Defined fuctions
               called=orddict:new(),            %Called functions
               calls=orddict:new(),             %Who calls who
               imported=[],                     %Actually imported functions
               func=[],                         %Current function
               warn_format=0,                   %Warn format calls
               warn_shadow=true,                %     shadowed variables
               warn_export=false,               %     all exported variables
               warn_unused=false,               %     unused variables
               warn_import=false,               %     unused imports
               errors=[],                       %Current errors
               warnings=[]                      %Current warnings
              }).

%% format_error(Error)
%%  Return a string describing the error.

format_error(undefined_module) ->
    "no module definition";
format_error({bad_module_name, M}) ->
    io_lib:format("bad module name '~s'", [M]);
format_error(redefine_module) ->
    "redefining module";
%%% format_error({redefine_mod_import, M, P}) ->
%%%     io_lib:format("module '~s' already imported from package '~s'",
%%%               [M, P]);

format_error(invalid_call) ->
    "invalid function call";
format_error(invalid_record) ->
    "invalid record expression";

format_error(no_generator) ->
    "list comprehension has no generator (perhaps \"|\" mistyped as \"||\"?)";

format_error({attribute,A}) ->
    io_lib:format("attribute '~w' after function definitions", [A]);
format_error({redefine_import,{bif,{F,A},M}}) ->
    io_lib:format("function ~w/~w already auto-imported from ~w",
                  [F,A,M]);
format_error({redefine_import,{{F,A},M}}) ->
    io_lib:format("function ~w/~w already imported from ~w", [F,A,M]);

format_error(export_all) ->
    "non-recommended option 'export_all' used";

format_error({unused_import,{{F,A},M}}) ->
    io_lib:format("import ~w:~w/~w is unused", [M,F,A]);
format_error({undefined_function,{F,A}}) ->
    io_lib:format("function ~w/~w undefined", [F,A]);
format_error({redefine_function,{F,A}}) ->
    io_lib:format("function ~w/~w already defined", [F,A]);
format_error({define_import,{F,A}}) ->
    io_lib:format("defining imported function ~w/~w", [F,A]);
format_error({unused_function,{F,A}}) ->
    io_lib:format("function ~w/~w is unused", [F,A]);
format_error({redefine_bif,{F,A}}) ->
    io_lib:format("defining BIF ~w/~w", [F,A]);
format_error(asm) -> "illegal asm";

format_error({obsolete, {M1, F1, A1}, {M2, F2, A2}}) ->
    io_lib:format("~p:~p/~p obsolete; use ~p:~p/~p", [M1, F1, A1, M2, F2, A2]);
format_error({obsolete, {M1, F1, A1}, String}) when list(String) ->
    io_lib:format("~p:~p/~p: ~s", [M1, F1, A1, String]);
format_error({reserved_for_future,K}) ->
    io_lib:format("atom ~w: future reserved keyword - rename or quote", [K]);

format_error(illegal_pattern) -> "illegal pattern";
format_error(illegal_expr) -> "illegal expression";
format_error(illegal_guard_expr) -> "illegal guard expression";

format_error({undefined_record,T}) ->
    io_lib:format("record ~w undefined", [T]);
format_error({redefine_record,T}) ->
    io_lib:format("record ~w already defined", [T]);
format_error({redefine_field,T,F}) ->
    io_lib:format("field ~w already defined in record ~w", [F,T]);
format_error({undefined_field,T,F}) ->
    io_lib:format("field ~w undefined in record ~w", [F,T]);
format_error(illegal_record_info) ->
    "illegal record info";
format_error({field_name_is_variable,T,F}) ->
    io_lib:format("field ~w is not an atom or _ in record ~w", [F,T]);
format_error({wildcard_in_update,T}) ->
    io_lib:format("meaningless use of _ in update of record ~w", [T]);

format_error({unbound_var,V}) ->
    io_lib:format("variable ~w is unbound", [V]);
format_error({unsafe_var,V,{What,Where}}) ->
    io_lib:format("variable ~w unsafe in ~w (line ~w)", [V,What,Where]);
format_error({exported_var,V,{What,Where}}) ->
    io_lib:format("variable ~w exported from ~w (line ~w)", [V,What,Where]);
format_error({shadowed_var,V,In}) ->
    io_lib:format("variable ~w shadowed in ~w", [V,In]);
format_error({unused_var, V}) ->
    io_lib:format("variable ~w is unused", [V]);

format_error({undefined_bittype,Type}) ->
    io_lib:format("bit type ~w undefined", [Type]);
format_error({bittype_mismatch,T1,T2,What}) ->
    io_lib:format("bit type mismatch (~s) between ~p and ~p", [What,T1,T2]);
format_error(illegal_bitsize) ->
    "illegal bit size";
format_error({bad_bitsize,Type}) ->
    io_lib:format("bad ~s bit size", [Type]);
format_error(unaligned_bitpat) ->
    "bit pattern not byte aligned";

format_error({format_error,{Fmt,Args}}) ->
    io_lib:format(Fmt, Args);
format_error({mnemosyne,What}) ->
    "mnemosyne " ++ What ++ ", missing transformation";

format_error({several_behaviours,Behaviours}) ->
    io_lib:format("several behaviours defined - ~p", [Behaviours]);
format_error({undefined_behaviour_func, {Func, Arity}}) ->
    io_lib:format("undefined call-back function ~w/~w", [Func, Arity]);
format_error({undefined_behaviour,Behaviour}) ->
    io_lib:format("behaviour ~w undefined", [Behaviour]);
format_error({undefined_behaviour_callbacks,Behaviour}) ->
    io_lib:format("behaviour ~w callback functions are undefined",
                  [Behaviour]);
format_error({ill_defined_behaviour_callbacks,Behaviour}) ->
    io_lib:format("behaviour ~w callback functions erroneously defined",
                  [Behaviour]).

%% Local functions that are somehow automatically generated.

pseudolocals() ->
    [{module_info,0}, {module_info,1}, {record_info,2}].

%%
%% Used by erl_eval.erl to check commands.
%% 

exprs(Exprs, BindingsList) ->
    Vs = map(fun({V,_Val}) -> {V,{bound,used}} end, BindingsList),
    Vt = orddict:from_list(Vs),
    {_Evt,St} = exprs(Exprs, Vt, start()),
    return_status(St).

used_vars(Exprs, BindingsList) ->
    Vs = map(fun({V,_Val}) -> {V,{bound,{unused,[]}}} end, BindingsList),
    Vt = orddict:from_list(Vs),
    {Evt,_St} = exprs(Exprs, Vt, start()),
    {ok, foldl(fun({V,{_,used}}, L) -> [V | L];
                  (_, L) -> L end, [], Evt)}.

%% module([Form]) ->
%% module([Form], FileName) ->
%% module([Form], FileName, [CompileOption]) ->
%%      {ok,[Warning]} | {error,[Error],[Warning]}
%%  Start processing a module. Define predefined functions and exports and
%%  apply_lambda/2 has been called to shut lint up. N.B. these lists are
%%  really all ordsets!

module(Forms) ->
    St = forms(Forms, start()),
    return_status(St).
    
module(Forms, FileName) ->
    St = forms(Forms, start(FileName)),
    return_status(St).

module(Forms, FileName, Opts) ->
    St = forms(Forms, start(FileName, Opts)),
    return_status(St).

%% start() -> State
%% start(FileName) -> State
%% start(FileName, [Option]) -> State

start() ->
    start("nofile", []).

start(File) ->
    start(File, []).

start(File, Opts) ->
    #lint{state=start,
          exports=ordsets:from_list([{module_info,0},
                                     {module_info,1}]),
          mod_imports=dict:from_list([{erlang,erlang}]),
          compile=Opts,
          %% Internal pseudo-functions must appear as defined/reached.
          defined=ordsets:from_list(pseudolocals()),
          called=orddict:from_list([{F, [0]} || F <- pseudolocals()]),
          calls=orddict:from_list([{{module_info,1}, pseudolocals()}]),
          warn_format=value_option(warn_format, 1, warn_format, 1,
                                   nowarn_format, 0, Opts),
          warn_shadow = bool_option(warn_shadow_vars, nowarn_shadow_vars,
                                    true, Opts),
          warn_export = bool_option(warn_export_vars, nowarn_export_vars,
                                    false, Opts),
          warn_unused = bool_option(warn_unused_vars, nowarn_unused_vars,
                                    false, Opts),
          warn_import = bool_option(warn_unused_import, nowarn_unused_import,
                                    false, Opts),
          errors=[{file,File}],
          warnings=[{file,File}]
         }.

%% return_status(State) ->
%%      {ok,[Warning]} | {error,[Error],[Warning]}
%%  Pack errors and warnings properly and return ok | error.

return_status(St) ->
    Ws = pack_errors(St#lint.warnings, [], []),
    case pack_errors(St#lint.errors, [], []) of
        [] -> {ok,Ws};
        Es -> {error,Es,Ws}
    end.

%% pack_errors([ErrD], [ErrD], [FileErrD]) -> [FileErrD].
%%  We know that the errors have been inserted in reverse order.

pack_errors([{file,_F}|Es], [], Ps) ->
    pack_errors(Es, [], Ps);
pack_errors([{file,F}|Es], Fes, Ps) ->
    pack_errors(Es, [], [{F,Fes}|Ps]);
pack_errors([E|Es], Fes, Ps) ->
    pack_errors(Es, [E|Fes], Ps);
pack_errors([], _Fes, Ps) -> Ps. 

%% add_error(ErrorDescriptor, State) -> State'
%% add_error(Line, Error, State) -> State'
%% add_warning(ErrorDescriptor, State) -> State'
%% add_warning(Line, Error, State) -> State'

add_error(E, St) -> St#lint{errors=[E|St#lint.errors]}.
add_error(Line, E, St) -> add_error({Line,erl_lint,E}, St).

place_warning([{L,erl_lint,_W}=T1 | Ws], {L1,erl_lint,_W1}=T2) when L1 < L ->
    [T1 | place_warning(Ws, T2)];
place_warning(Ws, W) -> [W | Ws].

add_warning(W, St) -> St#lint{warnings=place_warning(St#lint.warnings, W)}.
add_warning(Line, W, St) -> add_warning({Line,erl_lint,W}, St).

add_warning(true, L, W, St) -> add_warning(L, W, St); 
add_warning(false, _L, _W, St) -> St.

%% forms([Form], State) -> State'

forms(Forms, St) ->
    foldl(fun form/2, St, Forms).

%% form(Form, State) -> State'
%%  Check a form returning the updated State. Handle generic cases here.

form({error,E}, St)   -> add_error(E, St);
form({warning,W}, St) -> add_warning(W, St);
form({attribute,_L,file,{File,_Line}}, St) ->
    St#lint{errors=[{file,File}|St#lint.errors],
            warnings=[{file,File}|St#lint.warnings]};
form(Form, St) when St#lint.state == start ->
    start_state(Form, St);
form(Form, St) when St#lint.state == attribute ->
    attribute_state(Form, St);
form(Form, St) when St#lint.state == function ->
    function_state(Form, St).

%% start_state(Form, State) -> State'

start_state({attribute,L,module,M}, St) ->
    M1 = package_to_string(M),
    case packages:is_valid(M1) of
        true ->
            St#lint{state=attribute, module=list_to_atom(M1),
                    package=packages:strip_last(M1)};
        false ->
            St1 = add_error(L, {bad_module_name, M1}, St),
            St1#lint{state=attribute}
    end;
start_state(Form, St0) ->
    St1 = add_error(element(2, Form), undefined_module, St0),
    attribute_state(Form, St1#lint{state=attribute}).

%% attribute_state(Form, State) ->
%%      State'

attribute_state({attribute,L,module,_M}, St) ->
    add_error(L, redefine_module, St);
attribute_state({attribute,L,export,Es}, St) ->
    export(L, Es, St);
attribute_state({attribute,L,import,Is}, St) ->
    import(L, Is, St);
attribute_state({attribute,L,record,{Name,Fields}}, St) ->
    record_def(L, Name, Fields, St);
attribute_state({attribute,_La,compile,C}, St) when list(C) ->
    St#lint{compile=St#lint.compile ++ C};
attribute_state({attribute,_La,compile,C}, St) ->
    St#lint{compile=St#lint.compile ++ [C]};
attribute_state({attribute,La,asm,Func}, St) ->
    asm(La, Func, St);
attribute_state({attribute,La,behaviour,Behaviour}, St) ->
    St#lint{behaviour=St#lint.behaviour ++ [{La,Behaviour}]};
attribute_state({attribute,La,behavior,Behaviour}, St) ->
    St#lint{behaviour=[{La,Behaviour}|St#lint.behaviour]};
attribute_state({attribute,_L,_Other,_Val}, St) -> %Ignore others
    St;
attribute_state(Form, St) ->
    function_state(Form, St#lint{state=function}).

%% function_state(Form, State) ->
%%      State'
%%  Allow record definitions here!

function_state({attribute,L,record,{Name,Fields}}, St) ->
    record_def(L, Name, Fields, St);
function_state({attribute,La,asm,Func}, St) ->
    asm(La, Func, St);
function_state({attribute,La,Attr,_Val}, St) ->
    add_error(La, {attribute,Attr}, St);
function_state({function,L,N,A,Cs}, St) ->
    function(L, N, A, Cs, St);
function_state({rule,L,_N,_A,_Cs}, St) ->
    add_error(L, {mnemosyne,"rule"}, St);
function_state({eof,L}, St) -> eof(L, St).

%% eof(LastLine, State) ->
%%      State'

eof(Line, St0) ->
    %% Check that the behaviour attribute is valid.
    St1 = behaviour_check(St0#lint.behaviour, St0),
    %% Check for unreachable/unused functions.
    St2 = case member(export_all, St1#lint.compile) of
              true -> St1;
              false ->
                  %% Generate warnings.
                  Used = reached_functions(St1#lint.exports, St1#lint.calls),
                  func_warning(Line, unused_function,
                               ordsets:subtract(St1#lint.defined, Used),
                               St1)
          end,
    %% Check for unused imports.
    St3 = func_warning(St2#lint.warn_import, Line, unused_import,
                       ordsets:subtract(St2#lint.imports, St2#lint.imported),
                       St2),
    %% Check for undefined functions.
    Undef = foldl(fun (NA, Called) -> orddict:erase(NA, Called) end,
                  St3#lint.called, St3#lint.defined),
    orddict:fold(fun (NA, Ls, St) ->
                    foldl(fun (L, Sta) ->
                                  add_error(L, {undefined_function,NA}, Sta)
                          end, St, Ls)
            end, St3, Undef).

func_warning(true, Line, Type, Fs, St) -> func_warning(Line, Type, Fs, St);
func_warning(false, _Line, _Type, _Fs, St) -> St.

func_warning(Line, Type, Fs, St) ->
    foldl(fun (F, St0) -> add_warning(Line, {Type,F}, St0) end, St, Fs).

%% behaviour_check([{Line,Behaviour}], State) -> State'
%%  Check behaviours for existence and defined functions.

behaviour_check(Bs, St) ->
    Mult = length(Bs) > 1,                      %More than one behaviour?
    foldl(fun ({Line,B}, St0) ->
                  St1 = add_warning(Mult, Line, {several_behaviours,B}, St0),
                  {Bfs, St2} = behaviour_callbacks(Line, B, St1),
                  Missing = ordsets:subtract(ordsets:from_list(Bfs),
                                             St2#lint.exports),
                  func_warning(Line, undefined_behaviour_func,
                               Missing, St2)
          end, St, Bs).

behaviour_callbacks(Line, B, St0) ->
    case catch B:behaviour_info(callbacks) of
        {'EXIT', _Reason} ->
            St1 = add_warning(Line, {undefined_behaviour,B}, St0),
            {[], St1};
        Funcs when list(Funcs) ->
            All = all(fun({FuncName, Arity}) ->
                              if
                                  atom(FuncName), integer(Arity) ->
                                      true;
                                  true ->
                                      false
                              end;
                         (_Other) ->
                              false
                      end,
                      Funcs),
            if
                All==true ->
                    {Funcs, St0};
                true ->
                    St1 = add_warning(Line,
                                      {ill_defined_behaviour_callbacks,B},
                                      St0),
                    {[], St1}
            end;
        undefined ->
            St1 = add_warning(Line, {undefined_behaviour_callbacks,B}, St0),
            {[], St1};
        _Other ->
            St1 = add_warning(Line, {ill_defined_behaviour_callbacks,B}, St0),
            {[], St1}
    end.

%% asm(Line, Function, State) -> State'

asm(Line, {function,Name,Arity,_Code}, St) ->
    define_function(Line, Name, Arity, St);
asm(Line, _Func, St) ->
    add_error(Line, asm, St).

%% For storing the import list we use the orddict module. 
%% We know an empty set is [].

%% export(Line, Exports, State) -> State.
%%  Mark functions as exported, also as called from the export line.

export(Line, Es, #lint{exports=Es0,called=C0}=St) ->
    {Es1,C1} = foldl(fun (NA, {E,C}) ->
                             {ordsets:add_element(NA, E),
                              orddict:append(NA, Line, C)}
                     end,
                     {Es0,C0}, Es),
    St#lint{exports=Es1,called=C1}.

%% import(Line, Imports, State) -> State.
%% imported(Name, Arity, State) -> {yes,Module} | no.

import(Line, {Mod,Fs}, St) ->
    Mod1 = package_to_string(Mod),
    case packages:is_valid(Mod1) of
        true ->
            Mfs = ordsets:from_list(Fs),
            case check_imports(Line, Mfs, St#lint.imports) of
                [] ->
                    St#lint{imports=add_imports(list_to_atom(Mod1), Mfs,
                                                St#lint.imports)};
                Efs ->
                    foldl(fun (Ef, St0) ->
                                  add_error(Line, {redefine_import,Ef},
                                            St0)
                          end,
                          St, Efs)
            end;
        false ->
            add_error(Line, {bad_module_name, Mod1}, St)
    end;
import(Line, Mod, St) ->
    Mod1 = package_to_string(Mod),
    case packages:is_valid(Mod1) of
        true ->
            Key = list_to_atom(packages:last(Mod1)),
            Imps = St#lint.mod_imports,
%%%         case dict:is_key(Key, Imps) of
%%%             true ->
%%%                 M = packages:last(Mod1),
%%%                 P = packages:strip_last(Mod1),
%%%                 add_error(Line, {redefine_mod_import, M, P}, St);
%%%             false ->
%%%                 St#lint{mod_imports =
%%%                         dict:store(Key, list_to_atom(Mod1), Imps)}
%%%         end;
            St#lint{mod_imports = dict:store(Key, list_to_atom(Mod1),
                                             Imps)};
        false ->
            add_error(Line, {bad_module_name, Mod1}, St)
    end.

check_imports(_Line, Fs, Is) ->
    foldl(fun (F, Efs) ->
              case orddict:find(F, Is) of
                  {ok,Mod} -> [{F,Mod}|Efs];
                  error ->
                      {N,A} = F,
                      case erl_internal:bif(N, A) of
                          true ->
                              [{bif,F,erlang}|Efs];
                          false ->
                              Efs
                      end
              end end, [], Fs).

add_imports(Mod, Fs, Is) ->
    foldl(fun (F, Is0) -> orddict:store(F, Mod, Is0) end, Is, Fs).

imported(F, A, St) ->
    case orddict:find({F,A}, St#lint.imports) of
        {ok,Mod} -> {yes,Mod};
        error -> no
    end.

%% call_function(Line, Name, Arity, State) -> State.
%%  Add to both called and calls.

call_function(Line, F, A, #lint{called=Cd,calls=Cs,func=Func}=St) ->
    NA = {F,A},
    St#lint{called=orddict:append(NA, Line, Cd),
            calls=orddict:append(Func, NA, Cs)}.

%% reached_functions(RootSet, CallRef) -> [ReachedFunc].
%% reached_functions(RootSet, CallRef, [ReachedFunc]) -> [ReachedFunc].

reached_functions(Root, Ref) -> reached_functions(Root, Ref, []).

reached_functions([R|Rs], Ref, Reached) ->
    case ordsets:is_element(R, Reached) of
        true -> reached_functions(Rs, Ref, Reached);
        false ->
            Reached1 = ordsets:add_element(R, Reached), %It IS reached
            case orddict:find(R, Ref) of
                {ok,More} -> reached_functions(Rs ++ More, Ref, Reached1);
                error -> reached_functions(Rs, Ref, Reached1)
            end
    end;
reached_functions([], _Ref, Reached) -> Reached.

%% function(Line, Name, Arity, Clauses, State) -> State.

function(Line, Name, Arity, Cs, St0) ->
    St1 = define_function(Line, Name, Arity, St0#lint{func={Name,Arity}}),
    clauses(Cs, [], St1).

%% define_function(Line, Name, Arity, State) -> State.

define_function(Line, Name, Arity, St0) ->
    St1 = keyword_warning(Line, Name, St0),
    NA = {Name,Arity},
    case member(NA, St1#lint.defined) of
        true ->
            add_error(Line, {redefine_function,NA}, St1);
        false ->
            St2 = St1#lint{defined=ordsets:add_element(NA, St1#lint.defined)},
            St = case erl_internal:bif(Name, Arity) of
                      true ->
                         add_warning(Line, {redefine_bif,NA}, St2);
                     false -> St2
                  end,
            case imported(Name, Arity, St) of
                {yes,_M} -> add_error(Line, {define_import,NA}, St);
                no -> St
            end
    end.

%% clauses([Clause], VarTable, State) -> {VarTable, State}.

clauses(Cs, Vt, St) ->
    foldl(fun (C, St0) ->
                  {_,St1} = clause(C, Vt, St0),
                  St1
          end, St, Cs).

clause({clause,_Line,H,G,B}, Vt0, St0) ->
    {Hvt,_Binvt,St1} = head(H, Vt0, St0),
    %% Ignored Binvt since there are no really global variables
    %% that could have been used.
    Vt1 = vtupdate(Hvt, Vt0),
    {Gvt,St2} = guard(G, Vt1, St1),
    Vt2 = vtupdate(Gvt, Vt1),
    {Bvt,St3} = exprs(B, Vt2, St2),
    Upd = vtupdate(Bvt, Vt2),
    check_unused_vars(Upd, Vt0, St3).

%% head([HeadPattern], VarTable, State) ->
%%      {VarTable,BinVarTable,State}
%%  Check a patterns in head returning "all" variables. Not updating the
%%  known variable list will result in multiple error messages/warnings.

head(Ps, Vt, St0) ->
    head(Ps, Vt, Vt, St0).    % Old = Vt

head([P|Ps], Vt, Old, St0) ->
    {Pvt,Bvt1,St1} = pattern(P, Vt, Old, St0),
    {Psvt,Bvt2,St2} = head(Ps, Vt, Old, St1),
    {vtmerge_pat(Pvt, Psvt),vtmerge(Bvt1,Bvt2),St2};
head([], _Vt, _Env, St) -> {[],[],St}.

%% pattern(Pattern, VarTable, Old, State) -> {UpdVarTable,BinVarTable,State}.
%%  Check pattern return variables. Old is the set of variables used for
%%  deciding whether an occurrence is a binding occurrence or a use, and
%%  VarTable is the set of variables used for arguments to binary
%%  patterns. BinVarTable are used "binsize" variables (in <<X:Y>>, Y is a
%%  binsize variable).

pattern(P, Vt, St) ->
    pattern(P, Vt, Vt, St).    % Old = Vt

pattern({var,_Line,'_'}, _Vt, _Old, St) ->
    {[],[],St}; %Ignore anonymous variable
pattern({var,Line,V}, _Vt, Old, St) -> 
    {Vt1,St1} = pat_var(V, Line, Old, St),
    {Vt1,[],St1};
pattern({char,_Line,_C}, _Vt, _Old, St) -> {[],[],St};
pattern({integer,_Line,_I}, _Vt, _Old, St) -> {[],[],St};
pattern({float,_Line,_F}, _Vt, _Old, St) -> {[],[],St};
pattern({atom,Line,A}, _Vt, _Old, St) ->
    {[],[],keyword_warning(Line, A, St)};
pattern({string,_Line,_S}, _Vt, _Old, St) -> {[],[],St};
pattern({nil,_Line}, _Vt, _Old, St) -> {[],[],St};
pattern({cons,_Line,H,T}, Vt, Old,  St0) ->
    {Hvt,Bvt1,St1} = pattern(H, Vt, Old, St0),
    {Tvt,Bvt2,St2} = pattern(T, Vt, Old, St1),
    {vtmerge_pat(Hvt, Tvt),vtmerge(Bvt1,Bvt2),St2};
pattern({tuple,_Line,Ps}, Vt, Old, St) ->
    pattern_list(Ps, Vt, Old, St);
%%pattern({struct,_Line,_Tag,Ps}, Vt, Old, St) ->
%%    pattern_list(Ps, Vt, Old, St);
pattern({record_index,Line,Name,Field}, _Vt, _Old, St) ->
    {Vt1,St1} = 
        check_record(Line, Name, St,
                     fun (Dfs) ->
                             pattern_field(Field, Name, Dfs, St)
                     end),
    {Vt1,[],St1};
pattern({record_field,Line,_,_}=M, _Vt, _Old, St0) ->
    case expand_package(M, St0) of
        {error, St1} ->
            {[],[],add_error(Line, illegal_expr, St1)};
        {_, St1} ->
            {[],[],St1}
    end;
pattern({record,Line,Name,Pfs}, Vt, Old, St) ->
    case orddict:find(Name, St#lint.records) of
        {ok,Fields} -> 
            pattern_fields(Pfs, Name, Fields, Vt, Old, St);
        error -> {[],[],add_error(Line, {undefined_record,Name}, St)}
    end;
pattern({bin,Line,Fs}, Vt, Old, St) ->
    pattern_bin(Line, Fs, Vt, Old, St);
pattern({op,_Line,'++',{nil,_},R}, Vt, Old, St) ->
    pattern(R, Vt, Old, St);
pattern({op,_Line,'++',{cons,Li,{char,_L2,_C},T},R}, Vt, Old, St) ->
    pattern({op,Li,'++',T,R}, Vt, Old, St);    %Char unimportant here
pattern({op,_Line,'++',{cons,Li,{integer,_L2,_I},T},R}, Vt, Old, St) ->
    pattern({op,Li,'++',T,R}, Vt, Old, St);    %Weird, but compatible!
pattern({op,_Line,'++',{string,_Li,_S},R}, Vt, Old, St) ->
    pattern(R, Vt, Old, St);                   %String unimportant here
pattern({match,_Line,Pat1,Pat2}, Vt, Old, St0) ->
    {Lvt,Bvt1,St1} = pattern(Pat1, Vt, Old, St0),
    {Rvt,Bvt2,St2} = pattern(Pat2, Vt, Old, St1),
    {vtmerge_pat(Lvt, Rvt),vtmerge(Bvt1,Bvt2),St2};
%% Catch legal constant expressions, including unary +,-.
pattern(Pat, _Vt, _Old, St) ->
    case is_pattern_expr(Pat) of
        true -> {[],[],St};
        false -> {[],[],add_error(element(2, Pat), illegal_pattern, St)}
    end.

pattern_list(Ps, Vt, Old, St) ->
    foldl(fun (P, {Psvt,Bvt,St0}) ->
                  {Pvt,Bvt1,St1} = pattern(P, Vt, Old, St0),
                  {vtmerge_pat(Pvt, Psvt),vtmerge(Bvt,Bvt1),St1}
          end, {[],[],St}, Ps).

%% is_pattern_expr(Expression) ->
%%      true | false.
%%  Test if a general expression is a valid pattern expression.

is_pattern_expr(Expr) ->
    case is_pattern_expr_1(Expr) of
	false -> false;
	true ->
	    %% Expression is syntactically correct - make sure that it
	    %% also can be evaluated.
	    case erl_eval:partial_eval(Expr) of
		{integer,_,_} -> true;
		{char,_,_} -> true;
		{float,_,_} -> true;
		{atom,_,_} -> true;
		_ -> false
	    end
    end.

is_pattern_expr_1({char,_Line,_C}) -> true;
is_pattern_expr_1({integer,_Line,_I}) -> true;
is_pattern_expr_1({float,_Line,_F}) -> true;
is_pattern_expr_1({atom,_Line,_A}) -> true;
is_pattern_expr_1({tuple,_Line,Es}) ->
    all(fun is_pattern_expr/1, Es);
is_pattern_expr_1({nil,_Line}) -> true;
is_pattern_expr_1({cons,_Line,H,T}) ->
    case is_pattern_expr_1(H) of
        true -> is_pattern_expr_1(T);
        false -> false
    end;
is_pattern_expr_1({op,_Line,Op,A}) ->
    case erl_internal:arith_op(Op, 1) of
        true -> is_pattern_expr_1(A);
        false -> false
    end;
is_pattern_expr_1({op,_Line,Op,A1,A2}) ->
    case erl_internal:arith_op(Op, 2) of
        true -> all(fun is_pattern_expr/1, [A1,A2]);
        false -> false
    end;
is_pattern_expr_1(_Other) -> false.

%% pattern_bin(Line, [Element], VarTable, Old, State) -> 
%%           {UpdVarTable,BinVarTable,State}.
%%  Check a pattern group. BinVarTable are used binsize variables.

pattern_bin(Line, Es, Vt, Old, St0) ->
    {Sz,Esvt,Bvt,St1} = foldl(fun (E, Acc) ->
                                  pattern_element(E, Vt, Old, Acc)
                          end,
                          {0,[],[],St0}, Es),
    St2 = if integer(Sz), Sz rem 8 =/= 0 -> 
                  add_warning(Line,unaligned_bitpat, St1);
             true -> St1
          end,
    {Esvt,Bvt,St2}.

pattern_element({bin_element,Line,E,Sz0,Ts}, Vt, Old, {Size0,Esvt,Bvt,St0}) ->
    {Pevt,St1} = pat_bit_expr(E, Old, St0),
    {Sz1,Bvt1,St2} = pat_bit_size(Sz0, vtmerge(Vt, Esvt), St1),
    {Sz2,Bt,St3} = bit_type(Line, Sz1, Ts, St2),
    {Sz3,St4} = bit_size_check(Line, Sz2, Bt, St3),
    {Size1,St5} = add_bit_size(Line, Sz3, Size0, false, St4),
    {Size1,vtmerge_pat(Pevt, Esvt),vtmerge(Bvt, Bvt1), St5}.

%% pat_bit_expr(Pattern, OldVarTable, State) -> {UpdVarTable,State}.
%%  Check pattern bit expression, only allow really valid patterns!

pat_bit_expr({var,_,'_'}, _Old, St) -> {[],St};
pat_bit_expr({var,Ln,V}, Old, St) -> pat_var(V, Ln, Old, St);
pat_bit_expr({string,_,_}, _Old, St) -> {[],St};
pat_bit_expr({bin,L,_}, _Old, St) ->
    {[],add_error(L, illegal_pattern, St)};
pat_bit_expr(P, _Old, St) ->
    case is_pattern_expr(P) of
        true -> {[],St};
        false -> {[],add_error(element(2, P), illegal_pattern, St)}
    end.

%% pat_bit_size(Size, VarTable, State) -> {Value,UpdVarTable,State}.
%%  Check pattern size expression, only allow really valid sizes!

pat_bit_size(default, _Vt, St) -> {default,[],St};
pat_bit_size({atom,_Line,all}, _Vt, St) -> {all,[],St};
pat_bit_size({var,Lv,V}, Vt0, St0) ->
    {Bvt,St1} = pat_binsize_var(V, Lv, Vt0, St0),
    {unknown,Bvt,St1};
pat_bit_size(Size, _Vt, St) ->
    Line = element(2, Size),
    case is_pattern_expr(Size) of
        true ->
            case erl_eval:partial_eval(Size) of
                {integer,Line,I} -> {I,[],St};
                _Other -> {unknown,[],add_error(Line, illegal_bitsize, St)}
            end;
        false -> {unknown,[],add_error(Line, illegal_bitsize, St)}
    end.

%% expr_bin(Line, [Element], VarTable, State, CheckFun) -> {UpdVarTable,State}.
%%  Check an expression group.

expr_bin(Line, Es, Vt, St0, Check) ->
    {Sz,Esvt,St1} = foldl(fun (E, Acc) -> bin_element(E, Vt, Acc, Check) end,
                          {0,[],St0}, Es),
    St2 = if integer(Sz), Sz rem 8 =/= 0 -> 
                  add_warning(Line,unaligned_bitpat, St1);
              true -> St1
          end,
    {Esvt,St2}.

bin_element({bin_element,Line,E,Sz0,Ts}, Vt, {Size0,Esvt,St0}, Check) ->
    {Vt1,St1} = Check(E, Vt, St0),
    {Sz1,Vt2,St2} = bit_size(Sz0, Vt, St1, Check),
    {Sz2,Bt,St3} = bit_type(Line, Sz1, Ts, St2),
    {Sz3,St4} = bit_size_check(Line, Sz2, Bt, St3),
    {Size1,St5} = add_bit_size(Line, Sz3, Size0, true, St4),
    {Size1,vtmerge([Vt2,Vt1,Esvt]),St5}.

bit_size(default, _Vt, St, _Check) -> {default,[],St};
bit_size({atom,_Line,all}, _Vt, St, _Check) -> {all,[],St};
bit_size(Size, Vt, St, Check) ->
    %% Try to safely evaluate Size if constant to get size,
    %% otherwise just treat it as an expression.
    case is_gexpr(Size, St#lint.records) of
        true ->
            case erl_eval:partial_eval(Size) of
                {integer,_ILn,I} -> {I,[],St};
                _Other ->
                    {Evt,St1} = Check(Size, Vt, St),
                    {unknown,Evt,St1}
            end;
        false ->
            {Evt,St1} = Check(Size, Vt, St),
            {unknown,Evt,St1}
    end.

%% bit_type(Line, Size, TypeList, State) ->  {Size,#bittype,St}.
%%  Perform warning check on type and size.

bit_type(Line, Size0, Type, St) ->
    case erl_bits:set_bit_type(Size0, Type) of
        {ok,Size1,Bt} -> {Size1,Bt,St};
        {error,What} ->
            %% Flag error and generate a default.
            {ok,Size1,Bt} = erl_bits:set_bit_type(default, []),
            {Size1,Bt,add_error(Line, What, St)}
    end.

%% bit_size_check(Line, Size, BitType, State) -> {BitSize,State}.
%%  Do some checking & warnings on types
%%   float == 32 or 64
%%   list/binary sizes must be multiple of 8 

bit_size_check(_Line, unknown, _, St) -> {unknown,St};
bit_size_check(Line, all, #bittype{type=Type}, St) ->
    if
        Type == binary -> {all,St};
        true -> {unknown,add_error(Line, illegal_bitsize, St)}
    end;
bit_size_check(Line, Size, #bittype{type=Type,unit=Unit}, St) ->
    Sz = Unit * Size,                           %Total number of bits!
    St2 = elemtype_check(Line, Type, Sz, St),
    {Sz,St2}.
                    
elemtype_check(_Line, float, 32, St) -> St;
elemtype_check(_Line, float, 64, St) -> St;
elemtype_check(Line, float, _Size, St) ->
    add_warning(Line, {bad_bitsize,"float"}, St);
elemtype_check(Line, binary, N, St) when N rem 8 =/= 0 ->
    add_warning(Line, {bad_bitsize,"binary"}, St);
elemtype_check(_Line, _Type, _Size, St) ->  St.

%% add_bit_size(Line, ElementSize, BinSize, Build, State) -> {Size,State}.
%%  Add bits to group size.

add_bit_size(_Line, all, _Sz2, _B, St) -> {all,St};
add_bit_size(Line, Sz1, all, B, St) ->
    {all, if B == false, integer(Sz1), Sz1 =/= 0 ->
                 add_error(Line, illegal_bitsize, St);
             true -> St
         end};
add_bit_size(_Line, unknown, _Sz2, _B, St) -> {unknown,St};
add_bit_size(_Line, _Sz1, unknown, _B, St) -> {unknown,St};
add_bit_size(_Line, Sz1, Sz2, _B, St) -> {Sz1 + Sz2,St}.

%% guard([GuardTest], VarTable, State) ->
%%      {UsedVarTable,State}
%%  Check a guard, return all variables.

%% Disjunction of guard conjunctions
guard([L|R], Vt, St0) when list(L) ->
    {Gvt, St1} = guard_tests(L, Vt, St0),
    {Gsvt, St2} = guard(R, vtupdate(Gvt, Vt), St1),
    {vtupdate(Gvt, Gsvt),St2};
guard(L, Vt, St0) ->
    guard_tests(L, Vt, St0).

%% guard conjunction
guard_tests([G|Gs], Vt, St0) ->
    {Gvt,St1} = guard_test(G, Vt, St0),
    {Gsvt,St2} = guard_tests(Gs, vtupdate(Gvt, Vt), St1),
    {vtupdate(Gvt, Gsvt),St2};
guard_tests([], _Vt, St) -> {[],St}.

%% guard_test(Test, VarTable, State) ->
%%      {UsedVarTable,State'}
%%  Check one guard test, returns NewVariables.  We now allow more
%%  expressions in guards including the new is_XXX type tests, but
%%  only allow the old type tests at the top level.

%% Specially handle record type test here.
guard_test({call,Line,{atom,Lr,record},[E,A]}, Vt, St0) ->
    gexpr({call,Line,{atom,Lr,is_record},[E,A]}, Vt, St0);
guard_test({call,_Line,{atom,_La,F},As}=G, Vt, St0) ->
    {Asvt,St1} = gexpr_list(As, Vt, St0),       %Always check this.
    A = length(As),
    case erl_internal:type_test(F, A) of
        true when F =/= is_record -> {Asvt,St1};
        _ -> gexpr(G, Vt, St0)
    end;
guard_test(G, Vt, St) ->
    %% Everything else is a guard expression.
    gexpr(G, Vt, St).

%% gexpr(GuardExpression, VarTable, State) ->
%%      {UsedVarTable,State'}
%%  Check a guard expression, returns NewVariables.

gexpr({var,Line,V}, Vt, St) ->
    expr_var(V, Line, Vt, St);
gexpr({char,_Line,_C}, _Vt, St) -> {[],St};
gexpr({integer,_Line,_I}, _Vt, St) -> {[],St};
gexpr({float,_Line,_F}, _Vt, St) -> {[],St};
gexpr({atom,Line,A}, _Vt, St) ->
    {[],keyword_warning(Line, A, St)};
gexpr({string,_Line,_S}, _Vt, St) -> {[],St};
gexpr({nil,_Line}, _Vt, St) -> {[],St};
gexpr({cons,_Line,H,T}, Vt, St) ->
    gexpr_list([H,T], Vt, St);
gexpr({tuple,_Line,Es}, Vt, St) ->
    gexpr_list(Es, Vt, St);
%%gexpr({struct,_Line,_Tag,Es}, Vt, St) ->
%%    gexpr_list(Es, Vt, St);
gexpr({record_index,Line,Name,Field}, _Vt, St) ->
    check_record(Line, Name, St,
                 fun (Dfs) -> record_field(Field, Name, Dfs, St) end );
gexpr({record_field,Line,_,_}=M, _Vt, St0) ->
    case expand_package(M, St0) of
        {error, St1} ->
            {[],add_error(Line, illegal_expr, St1)};
        {_, St1} ->
            {[], St1}
    end;
gexpr({record_field,Line,Rec,Name,Field}, Vt, St0) ->
    {Rvt,St1} = gexpr(Rec, Vt, St0),
    {Fvt,St2} = check_record(Line, Name, St1,
                             fun (Dfs) ->
                                     record_field(Field, Name, Dfs, St1)
                             end),
    {vtmerge(Rvt, Fvt),St2};
gexpr({record,Line,Name,Inits}, Vt, St) ->
    check_record(Line, Name, St,
                 fun (Dfs) ->
                         ginit_fields(Inits, Line, Name, Dfs, Vt, St)
                 end);
gexpr({bin,Line,Fs}, Vt,St) ->
    expr_bin(Line, Fs, Vt, St, fun gexpr/3);
gexpr({call,_Line,{atom,_Lr,is_record},[E,{atom,Ln,Name}]}, Vt, St0) ->
    {Rvt,St1} = gexpr(E, Vt, St0),
    {Rvt,exist_record(Ln, Name, St1)};
gexpr({call,Line,{atom,_Lr,is_record},[E,R]}, Vt, St0) ->
    {Asvt,St1} = gexpr_list([E,R], Vt, St0),
    {Asvt,add_error(Line, illegal_guard_expr, St1)};
gexpr({call,Line,{remote,_Lr,{atom,_Lm,erlang},{atom,Lf,is_record}},[E,A]}, 
      Vt, St0) ->
    gexpr({call,Line,{atom,Lf,is_record},[E,A]}, Vt, St0);
gexpr({call,Line,{atom,_La,F},As}, Vt, St0) ->
    {Asvt,St1} = gexpr_list(As, Vt, St0),
    A = length(As),
    case erl_internal:guard_bif(F, A) of
        true -> {Asvt,St1};
        false -> {Asvt,add_error(Line, illegal_guard_expr, St1)}
    end;
gexpr({call,Line,{remote,_Lr,{atom,_Lm,erlang},{atom,_Lf,F}},As}, Vt, St0) ->
    {Asvt,St1} = gexpr_list(As, Vt, St0),
    A = length(As),
    case erl_internal:guard_bif(F, A) orelse is_gexpr_op(F, A) of
        true -> {Asvt,St1};
        false -> {Asvt,add_error(Line, illegal_guard_expr, St1)}
    end;
gexpr({call,L,{tuple,Lt,[{atom,Lm,erlang},{atom,Lf,F}]},As}, Vt, St) ->
    gexpr({call,L,{remote,Lt,{atom,Lm,erlang},{atom,Lf,F}},As}, Vt, St);
gexpr({op,Line,Op,A}, Vt, St0) ->
    {Avt,St1} = gexpr(A, Vt, St0),
    case is_gexpr_op(Op, 1) of
        true -> {Avt,St1};
        false -> {Avt,add_error(Line, illegal_guard_expr, St1)}
    end;
gexpr({op,Line,Op,L,R}, Vt, St0) ->
    {Avt,St1} = gexpr_list([L,R], Vt, St0),
    case is_gexpr_op(Op, 2) of
        true -> {Avt,St1};
        false -> {Avt,add_error(Line, illegal_guard_expr, St1)}
    end;
%% Everything else is illegal! You could put explicit tests here to
%% better error diagnostics.
gexpr(E, _Vt, St) ->
    {[],add_error(element(2, E), illegal_guard_expr, St)}.

%% gexpr_list(Expressions, VarTable, State) ->
%%      {UsedVarTable,State'}

gexpr_list(Es, Vt, St) ->
    foldl(fun (E, {Esvt,St0}) ->
                  {Evt,St1} = gexpr(E, Vt, St0),
                  {vtmerge(Evt, Esvt),St1}
          end, {[],St}, Es).

%% is_guard_test(Expression) -> true | false.
%%  Test if a general expression is a guard test.
is_guard_test(E) ->
    is_guard_test(E, []).

%% is_guard_test(Expression, RecordDefs) -> true | false.
is_guard_test({call,Line,{atom,Lr,record},[E,A]}, RDs) ->
    is_gexpr({call,Line,{atom,Lr,is_record},[E,A]}, RDs);
is_guard_test({call,_Line,{atom,_La,Test},As}=Call, RDs) ->
    case erl_internal:type_test(Test, length(As)) of
        true -> is_gexpr_list(As, RDs);
        false -> is_gexpr(Call, RDs)
    end;
is_guard_test(G, RDs) ->
    %%Everything else is a guard expression.
    is_gexpr(G, RDs).

%% is_guard_expr(Expression) -> true | false.
%%  Test if an expression is a guard expression.

is_guard_expr(E) -> is_gexpr(E, []). 

is_gexpr({var,_L,_V}, _RDs) -> true;
is_gexpr({char,_L,_C}, _RDs) -> true;
is_gexpr({integer,_L,_I}, _RDs) -> true;
is_gexpr({float,_L,_F}, _RDs) -> true;
is_gexpr({atom,_L,_A}, _RDs) -> true;
is_gexpr({string,_L,_S}, _RDs) -> true;
is_gexpr({nil,_L}, _RDs) -> true;
is_gexpr({cons,_L,H,T}, RDs) -> is_gexpr_list([H,T], RDs);
is_gexpr({tuple,_L,Es}, RDs) -> is_gexpr_list(Es, RDs);
%%is_gexpr({struct,_L,_Tag,Es}, RDs) ->
%%    is_gexpr_list(Es, RDs);
is_gexpr({record_index,_L,_Name,Field}, RDs) ->
    is_gexpr(Field, RDs);
is_gexpr({record_field,_L,_,_}=M, _RDs) ->
    case erl_parse:package_segments(M) of
        error -> false;
      _ -> true
    end;
is_gexpr({record_field,_L,Rec,_Name,Field}, RDs) ->
    is_gexpr_list([Rec,Field], RDs);
is_gexpr({record,L,Name,Inits}, RDs) ->
    is_gexpr_fields(Inits, L, Name, RDs);
is_gexpr({bin,_L,Fs}, RDs) ->
    all(fun ({bin_element,_Line,E,Sz,_Ts}) -> 
                is_gexpr(E, RDs) and (Sz == default orelse is_gexpr(Sz, RDs))
        end, Fs);
is_gexpr({call,_L,{atom,_Lf,F},As}, RDs) ->
    A = length(As),
    case erl_internal:guard_bif(F, A) of
        true -> is_gexpr_list(As, RDs);
        false -> false
    end;
is_gexpr({call,_L,{remote,_Lr,{atom,_Lm,erlang},{atom,_Lf,F}},As}, RDs) ->
    A = length(As),
    case erl_internal:guard_bif(F, A) orelse is_gexpr_op(F, A) of
        true -> is_gexpr_list(As, RDs);
        false -> false
    end;
is_gexpr({call,L,{tuple,Lt,[{atom,Lm,erlang},{atom,Lf,F}]},As}, RDs) ->
    is_gexpr({call,L,{remote,Lt,{atom,Lm,erlang},{atom,Lf,F}},As}, RDs);
is_gexpr({op,_L,Op,A}, RDs) ->
    case is_gexpr_op(Op, 1) of
        true -> is_gexpr(A, RDs);
        false -> false
    end;
is_gexpr({op,_L,Op,A1,A2}, RDs) ->
    case is_gexpr_op(Op, 2) of
        true -> is_gexpr_list([A1,A2], RDs);
        false -> false
    end;
is_gexpr(_Other, _RDs) -> false.

is_gexpr_op(Op, A) ->
    case catch erl_internal:op_type(Op, A) of
        arith -> true;
        bool -> true;
        comp -> true;
        _Other -> false
    end.

is_gexpr_list(Es, RDs) -> all(fun (E) -> is_gexpr(E, RDs) end, Es).

is_gexpr_fields(Fs, L, Name, RDs) ->
    IFs = case orddict:find(Name, RDs) of
              {ok,Fields} -> init_fields(Fs, L, Fields);
              error  -> Fs
          end,
    all(fun ({record_field,_Lf,_Name,V}) -> is_gexpr(V, RDs);
            (_Other) -> false end, IFs).

%% exprs(Sequence, VarTable, State) ->
%%      {UsedVarTable,State'}
%%  Check a sequence of expressions, return all variables.

exprs([E|Es], Vt, St0) ->
    {Evt,St1} = expr(E, Vt, St0),
    {Esvt,St2} = exprs(Es, vtupdate(Evt, Vt), St1),
    {vtupdate(Evt, Esvt),St2};
exprs([], _Vt, St) -> {[],St}.

%% expr(Expression, VarTable, State) ->
%%      {UsedVarTable,State'}
%%  Check an expression, returns NewVariables. Assume naive users and
%%  mark illegally exported variables, e.g. from catch, as unsafe to better
%%  show why unbound.

expr({var,Line,V}, Vt, St) ->
    expr_var(V, Line, Vt, St);
expr({char,_Line,_C}, _Vt, St) -> {[],St};
expr({integer,_Line,_I}, _Vt, St) -> {[],St};
expr({float,_Line,_F}, _Vt, St) -> {[],St};
expr({atom,Line,A}, _Vt, St) ->
    {[],keyword_warning(Line, A, St)};
expr({string,_Line,_S}, _Vt, St) -> {[],St};
expr({nil,_Line}, _Vt, St) -> {[],St};
expr({cons,_Line,H,T}, Vt, St) ->
    expr_list([H,T], Vt, St);
expr({lc,Line,E,Qs}, Vt0, St0) ->
    {Vt1, Uvt, St1} = lc_quals(Qs, Vt0, St0),
    {Evt,St2} = expr(E, Vt1, St1),
    Vt2 = vtupdate(Evt, Vt1),
    %% Shadowed global variables.
    {_,St3} = check_old_unused_vars(Vt2, Uvt, St2),
    %% There may be local variables in Uvt that are not global.
    {_,St4} = check_unused_vars(Uvt, Vt0, St3),
    %% Local variables that have not been shadowed.
    {_,St5} = check_unused_vars(Vt2, Vt0, St4),
    Vt3 = vtmerge(vtsubtract(Vt2, Uvt), Uvt),
    St = warn_no_generator(Line, Qs, St5),
    {vtold(Vt3, Vt0),St};                      %Don't export local variables
expr({tuple,_Line,Es}, Vt, St) ->
    expr_list(Es, Vt, St);
%%expr({struct,Line,Tag,Es}, Vt, St) ->
%%    expr_list(Es, Vt, St);
expr({record_index,Line,Name,Field}, _Vt, St) ->
    check_record(Line, Name, St,
                 fun (Dfs) -> record_field(Field, Name, Dfs, St) end);
expr({record,Line,Name,Inits}, Vt, St) ->
    check_record(Line, Name, St,
                 fun (Dfs) -> init_fields(Inits, Line, Name, Dfs, Vt, St) end);
expr({record_field,Line,_,_}=M, _Vt, St0) ->
    case expand_package(M, St0) of
        {error, St1} ->
            {[],add_error(Line, illegal_expr, St1)};
        {_, St1} ->
            {[], St1}
    end;
expr({record_field,Line,Rec,Name,Field}, Vt, St0) ->
    {Rvt,St1} = record_expr(Line, Rec, Vt, St0),
    {Fvt,St2} = check_record(Line, Name, St1,
                             fun (Dfs) ->
                                     record_field(Field, Name, Dfs, St1)
                             end),
    {vtmerge(Rvt, Fvt),St2};
expr({record,Line,Rec,Name,Upds}, Vt, St0) ->
    {Rvt,St1} = record_expr(Line, Rec, Vt, St0),
    {Usvt,St2} = check_record(Line, Name, St1,
                          fun (Dfs) ->
                                  update_fields(Upds, Name, Dfs, Vt, St1)
                          end ),
    case has_wildcard_field(Upds) of
        true -> {[],add_error(Line, {wildcard_in_update,Name}, St2)};
        false -> {vtmerge(Rvt, Usvt),St2}
    end;
expr({bin,Line,Fs}, Vt, St) ->
    expr_bin(Line, Fs, Vt, St, fun expr/3);
expr({block,_Line,Es}, Vt, St) ->
    %% Unfold block into a sequence.
    exprs(Es, Vt, St);
expr({'if',Line,Cs}, Vt, St) ->
    icrt_clauses(Cs, {'if',Line}, Vt, St);
expr({'case',Line,E,Cs}, Vt, St0) ->
    {Evt,St1} = expr(E, Vt, St0),
    {Cvt,St2} = icrt_clauses(Cs, {'case',Line}, vtupdate(Evt, Vt), St1),
    {vtmerge(Evt, Cvt),St2};
expr({'receive',Line,Cs}, Vt, St) ->
    icrt_clauses(Cs, {'receive',Line}, Vt, St);
expr({'receive',Line,Cs,To,ToEs}, Vt, St0) ->
    %% Are variables from the timeout expression visible in the clauses? NO!
    {Tvt,St1} = expr(To, Vt, St0),
    {Tevt,St2} = exprs(ToEs, Vt, St1),
    {Cvt,St3} = icrt_clauses(Cs, Vt, St2),
    %% Csvts = [vtnew(Tevt, Vt)|Cvt],           %This is just NEW variables!
    Csvts = [Tevt|Cvt],
    {Rvt,St4} = icrt_export(Csvts, Vt, {'receive',Line}, St3),
    {vtmerge([Tvt,Tevt,Rvt]),St4};
expr({'fun',Line,Body}, Vt, St) ->
    %%No one can think funs export!
    case Body of
        {clauses,Cs} ->
            {Bvt, St1} = fun_clauses(Cs, Vt, St),
            {vtupdate(Bvt, Vt), St1};
        {function,F,A} ->
            %% N.B. Only allows BIFs here as well, NO IMPORTS!!
            case erl_internal:bif(F, A) of
                true -> {[],St};
                false -> {[],call_function(Line, F, A, St)}
            end
    end;
expr({call,Line,{remote,_Lr,M,F},As}, Vt, St0) ->
    case expand_package(M, St0) of
        {error, _} ->
            expr_list([M,F|As], Vt, St0);
        {{atom,_La,M1}, St1} ->
            case F of
                {atom,Lf,F1} ->
                    St2 = keyword_warning(Lf, F1, St1),
                    St3 = check_remote_function(Line, M1, F1, As, St2),
                    expr_list(As, Vt, St3);
                _ ->
                    expr_list([F|As], Vt, St1)
            end
    end;
expr({call,Line,{atom,La,F},As}, Vt, St0) ->
    St1 = keyword_warning(La, F, St0),
    {Asvt,St2} = expr_list(As, Vt, St1),
    A = length(As),
    case erl_internal:bif(F, A) of
        true -> {Asvt,St2};
        false ->
            {Asvt,case imported(F, A, St2) of
                      {yes,M} ->
                          St3 = check_remote_function(Line, M, F, As, St2),
                          St3#lint{imported=ordsets:add_element({{F,A},M},
                                                                St3#lint.imported)};
                      no ->
                          case {F,A} of
                              {record_info,2} ->
                                  check_record_info_call(Line,La,As,St2);
                              N when N == St2#lint.func -> St2;
                              _ -> call_function(Line, F, A, St2)
                          end
                  end}
    end;
expr({call,Line,{record_field,_,_,_}=F,As}, Vt, St0) ->
    case expand_package(F, St0) of
        {error, _} ->
	    expr_list([F|As], Vt, St0);
        {A, St1} ->
	    expr({call,Line,A,As}, Vt, St1)
    end;
expr({call,Line,F,As}, Vt, St0) ->
    St = warn_invalid_call(Line,F,St0),
    expr_list([F|As], Vt, St);                  %They see the same variables
expr({'try',Line,Es,Scs,Ccs}, Vt, St0) ->
    %% No new variables are added in exprs, flag new variables as unsafe.
    {Evt0,St1} = exprs(Es, Vt, St0),
    Uvt = vtunsafe(vtnames(vtnew(Evt0, Vt)), {'try',Line}, []),
    Evt1 = vtupdate(Uvt, vtupdate(Evt0, Vt)),
    %% However, these clauses can export!
    {Svt,St2} = icrt_clauses(Scs, Evt1, St1),
    {Cvt,St3} = icrt_clauses(Ccs, Evt1, St2),
    {Rvt,St4} = icrt_export(Svt ++ Cvt, Vt, {'try',Line}, St3),
    {vtmerge(Evt1, Rvt),St4};
expr({'catch',Line,E}, Vt, St0) ->
    %% No new variables added, flag new variables as unsafe.
    {Evt,St1} = expr(E, Vt, St0),
    Uvt = vtunsafe(vtnames(vtnew(Evt, Vt)), {'catch',Line}, []),
    {vtupdate(Uvt,vtupdate(Evt, Vt)),St1};
expr({match,_Line,P,E}, Vt, St0) ->
    {Evt,St1} = expr(E, Vt, St0),
    {Pvt,Bvt,St2} = pattern(P, vtupdate(Evt, Vt), St1),
    {vtupdate(Bvt, vtmerge(Evt, Pvt)),St2};
%% No comparison or boolean operators yet.
expr({op,_Line,_Op,A}, Vt, St) ->
    expr(A, Vt, St);
expr({op,Line,Op,L,R}, Vt, St0) when Op == 'orelse'; Op == 'andalso' ->
    {Evt1,St1} = expr(L, Vt, St0),
    Vt1 = vtupdate(Evt1, Vt),
    {Evt2,St2} = expr(R, Vt1, St1),
    Vt2 = vtmerge(Evt2, Vt1),
    {Vt3,St3} = icrt_export([Vt1,Vt2], Vt1, {Op,Line},St2),
    {vtmerge(Evt1, Vt3),St3};
expr({op,_Line,_Op,L,R}, Vt, St) ->
    expr_list([L,R], Vt, St);                   %They see the same variables
%% The following are not allowed to occur anywhere!
expr({remote,Line,_M,_F}, _Vt, St) ->
    {[],add_error(Line, illegal_expr, St)};
expr({'query',Line,_Q}, _Vt, St) ->
    {[],add_error(Line, {mnemosyne,"query"}, St)}.

%% expr_list(Expressions, Variables, State) ->
%%      {UsedVarTable,State}

expr_list(Es, Vt, St) ->
    foldl(fun (E, {Esvt,St0}) ->
                  {Evt,St1} = expr(E, Vt, St0),
                  {vtmerge(Evt, Esvt),St1}
          end, {[],St}, Es).

record_expr(Line, Rec, Vt, St0) ->
    St1 = warn_invalid_record(Line, Rec, St0),
    expr(Rec, Vt, St1).

%% warn_no_generator(Line, Qs, State0) -> State
%% Adds warning if a list comprehension has no generator.

warn_no_generator(_, [{generate,_,_,_}|_], St) -> St;
warn_no_generator(Line, [_|T], St) -> warn_no_generator(Line, T, St);
warn_no_generator(Line, [], St) -> add_warning(Line, no_generator, St).

%% warn_invalid_record(Line, Record, State0) -> State
%% Adds warning if the record is invalid.

warn_invalid_record(Line, R, St) ->
    case is_valid_record(R) of
        true -> St;
        false -> add_warning(Line, invalid_record, St)
    end.

%% is_valid_record(Record) -> bool()

is_valid_record(Rec) ->
    case Rec of
        {char, _, _} -> false;
        {integer, _, _} -> false;
        {float, _, _} -> false;
        {atom, _, _} -> false;
        {string, _, _} -> false;
        {cons, _, _, _} -> false;
        {nil, _} -> false;
        {lc, _, _, _} -> false;
        {record_index, _, _, _} -> false;
        {'fun', _, _} -> false;
        _ -> true
    end.

%% warn_invalid_call(Line, Call, State0) -> State
%% Adds warning if the call is invalid.

warn_invalid_call(Line, F, St) ->
    case is_valid_call(F) of
        true -> St;
        false -> add_warning(Line, invalid_call, St)
    end.

%% is_valid_call(Call) -> boolean()

is_valid_call(Call) ->
    case Call of
        {char, _, _} -> false;
        {integer, _, _} -> false;
        {float, _, _} -> false;
        {string, _, _} -> false;
        {cons, _, _, _} -> false;
        {nil, _} -> false;
        {lc, _, _, _} -> false;
        {record_index, _, _, _} -> false;
        {tuple, _, Exprs} when length(Exprs) /= 2 -> false;
        _ -> true
    end.

%% record_def(Line, RecordName, [RecField], State) -> State.
%%  Add a record definition if it does not already exist. Normalise
%%  so that all fields have explicit initial value.

record_def(Line, Name, Fs0, St0) ->
    case orddict:is_key(Name, St0#lint.records) of
        true -> add_error(Line, {redefine_record,Name}, St0);
        false ->
            {Fs1,St1} = def_fields(normalise_fields(Fs0), Name, St0),
            St1#lint{records=orddict:store(Name, Fs1, St1#lint.records)}
    end.

%% def_fields([RecDef], RecordName, State) -> {[DefField],State}.
%%  Check (normalised) fields for duplicates.  Return unduplicated
%%  record and set State.

def_fields(Fs0, Name, St0) ->
    foldl(fun ({record_field,Lf,{atom,La,F},V}, {Fs,St}) ->
                  case exist_field(F, Fs) of
                      true -> {Fs,add_error(Lf, {redefine_field,Name,F}, St)};
                      false -> {[{record_field,Lf,{atom,La,F},V}|Fs],St}
                  end
          end, {[],St0}, Fs0).

%% normalise_fields([RecDef]) -> [Field].
%%  Normalise the field definitions to always have a default value. If
%%  none has been given then use 'undefined'.

normalise_fields(Fs) ->
    map(fun ({record_field,Lf,Field}) ->
                {record_field,Lf,Field,{atom,Lf,undefined}};
            (F) -> F end, Fs).

%% exist_record(Line, RecordName, State) -> State.
%%  Check if a record exists.  Set State.

exist_record(Line, Name, St) ->
    case orddict:is_key(Name, St#lint.records) of
        true -> St;
        false -> add_error(Line, {undefined_record,Name}, St)
    end.

%% check_record(Line, RecordName, State, CheckFun) ->
%%      {UpdVarTable, State}.
%%  The generic record checking function, first checks that the record
%%  exists then calls the specific check function.  N.B. the check
%%  function can safely assume that the record exists.
%%
%%  The check function is called:
%%      CheckFun(RecordDefFields)
%%  and must return
%%      {UpdatedVarTable,State}

check_record(Line, Name, St, CheckFun) ->
    case orddict:find(Name, St#lint.records) of
        {ok,Fields} -> CheckFun(Fields);
        error -> {[],add_error(Line, {undefined_record,Name}, St)}
    end.

%%% Record check functions.

%% check_fields([ChkField], RecordName, [RecDefField], VarTable, State, CheckFun) ->
%%      {UpdVarTable,State}.

check_fields(Fs, Name, Fields, Vt, St0, CheckFun) ->
    {_SeenFields,Uvt,St1} =
        foldl(fun (Field, {Sfsa,Vta,Sta}) ->
                      {Sfsb,{Vtb,Stb}} = check_field(Field, Name, Fields,
                                                     Vt, Sta, Sfsa, CheckFun),
                      {Sfsb,vtmerge_pat(Vta, Vtb),Stb}
              end, {[],[],St0}, Fs),
    {Uvt,St1}.

check_field({record_field,Lf,{atom,La,F},Val}, Name, Fields,
            Vt, St, Sfs, CheckFun) ->
    case member(F, Sfs) of
        true -> {Sfs,{Vt,add_error(Lf, {redefine_field,Name,F}, St)}};
        false ->
            {[F|Sfs],
             case find_field(F, Fields) of
                 {ok,_I} -> CheckFun(Val, Vt, St);
                 error -> {[],add_error(La, {undefined_field,Name,F}, St)}
             end}
    end;
check_field({record_field,_Lf,{var,_La,'_'},Val}, _Name, _Fields,
            Vt, St, Sfs, CheckFun) ->
    {Sfs,CheckFun(Val, Vt, St)};
check_field({record_field,_Lf,{var,La,V},_Val}, Name, _Fields,
            Vt, St, Sfs, _CheckFun) ->
    {Sfs,{Vt,add_error(La, {field_name_is_variable,Name,V}, St)}}.

%% pattern_field(Field, RecordName, [RecDefField], State) ->
%%      {UpdVarTable,State}.
%%  Test if record RecordName has field Field. Set State.

pattern_field({atom,La,F}, Name, Fields, St) ->
    case find_field(F, Fields) of
        {ok,_I} -> {[],St};
        error -> {[],add_error(La, {undefined_field,Name,F}, St)}
    end.

%% pattern_fields([PatField],RecordName,[RecDefField],VarTable,Old,State) ->
%%      {UpdVarTable,BinVarTable,State}.

pattern_fields(Fs, Name, Fields, Vt0, Old, St0) ->
    CheckFun = fun (Val, Vt, St) -> pattern(Val, Vt, Old, St) end,
    {_SeenFields,Uvt,Bvt1,St1} =
        foldl(fun (Field, {Sfsa,Vta,Bvt,Sta}) ->
                      case check_field(Field, Name, Fields,
                                       Vt0, Sta, Sfsa, CheckFun) of
                          {Sfsb,{Vtb,Stb}} ->
                              {Sfsb,vtmerge_pat(Vta, Vtb),[],Stb};
                          {Sfsb,{Vtb,Bvt2,Stb}} ->
                              {Sfsb,vtmerge_pat(Vta, Vtb),
                               vtmerge(Bvt,Bvt2),Stb}
                      end
              end, {[],[],[],St0}, Fs),
    {Uvt,Bvt1,St1}.

%% record_field(Field, RecordName, [RecDefField], State) ->
%%      {UpdVarTable,State}.
%%  Test if record RecordName has field Field. Set State.

record_field({atom,La,F}, Name, Fields, St) ->
    case find_field(F, Fields) of
        {ok,_I} -> {[],St};
        error -> {[],add_error(La, {undefined_field,Name,F}, St)}
    end.

%% init_fields([InitField], InitLine, RecordName, [DefField], VarTable, State) ->
%%      {UpdVarTable,State}.
%% ginit_fields([InitField], InitLine, RecordName, [DefField], VarTable, State) ->
%%      {UpdVarTable,State}.
%%  Check record initialisation.  Create an initialisation list by
%%  removing the explicit initialisations from the definition fields
%%  and then appending the initialisations.  The line numbers of the
%%  remaining definition fields are changed to the line of the current
%%  initialisation for error messages.  This is then passed on to
%%  check_fields for checking.

init_fields(Ifs, Line, Name, Dfs, Vt, St) ->
    Inits = init_fields(Ifs, Line, Dfs),
    check_fields(Inits, Name, Dfs, Vt, St, fun expr/3).

ginit_fields(Ifs, Line, Name, Dfs, Vt, St) ->
    Inits = init_fields(Ifs, Line, Dfs),
    check_fields(Inits, Name, Dfs, Vt, St, fun gexpr/3).

init_fields(Ifs, Line, Dfs) ->
    [ {record_field,Line,{atom,Line,F},copy_expr(Di, Line)} ||
        {record_field,_Lf,{atom,_La,F},Di} <- Dfs,
        not exist_field(F, Ifs) ] ++ Ifs.

%% update_fields(UpdFields, RecordName, RecDefFields, VarTable, State) ->
%%      {UpdVarTable,State}

update_fields(Ufs, Name, Dfs, Vt, St) ->
    check_fields(Ufs, Name, Dfs, Vt, St, fun expr/3).

%% exist_field(FieldName, [Field]) -> bool().
%%  Find a record field in a field list.

exist_field(F, [{record_field,_Lf,{atom,_La,F},_Val}|_Fs]) -> true;
exist_field(F, [_|Fs]) -> exist_field(F, Fs);
exist_field(_F, []) -> false.

%% find_field(FieldName, [Field]) -> {ok,Val} | error.
%%  Find a record field in a field list.

find_field(_F, [{record_field,_Lf,{atom,_La,_F},Val}|_Fs]) -> {ok,Val};
find_field(F, [_|Fs]) -> find_field(F, Fs);
find_field(_F, []) -> error.

%% icrt_clauses(Clauses, In, ImportVarTable, State) ->
%%      {NewVts,State}.

icrt_clauses(Cs, In, Vt, St0) ->
    {Csvt,St1} = icrt_clauses(Cs, Vt, St0),
    icrt_export(Csvt, Vt, In, St1).

icrt_export(Csvt, Vt, In, St) ->
    Vt1 = vtmerge(Csvt),
    All = ordsets:subtract(vintersection(Csvt), vtnames(Vt)),
    Some = ordsets:subtract(vtnames(Vt1), vtnames(Vt)),
    Xvt = vtexport(All, In, []),
    Evt = vtunsafe(ordsets:subtract(Some, All), In, Xvt),
    Unused = vtmerge(map(fun (Vt0) -> unused_vars(Vt0, Vt, St) end, Csvt)),
    %% Exported and unsafe variables may be unused:
    Uvt = vtmerge(Evt, Unused),
    %% Make exported and unsafe unused variables unused in subsequent code:
    Vt2 = vtmerge(Uvt, vtsubtract(Vt1, Uvt)),
    {Vt2,St}.
    
%% icrt_clauses(Clauses, ImportVarTable, State) ->
%%      {NewVts,State}.

icrt_clauses(Cs, Vt, St) ->
    mapfoldl(fun (C, St0) -> icrt_clause(C, Vt, St0) end, St, Cs).

icrt_clause({clause,_Line,H,G,B}, Vt0, St0) ->
    {Hvt,Binvt,St1} = head(H, Vt0, St0),
    Vt1 = vtupdate(Hvt, vtupdate(Binvt, Vt0)),
    {Gvt,St2} = guard(G, Vt1, St1),
    Vt2 = vtupdate(Gvt, Vt1),
    {Bvt,St3} = exprs(B, Vt2, St2),
    {vtupdate(Bvt, Vt2),St3}.

%% lc_quals(Qualifiers, ImportVarTable, State) ->
%%      {VarTable,ShadowedVarTable,State}
%%  Test list comprehension qualifiers, returns all variables. Allow
%%  filters to be both guard tests and general expressions, but the errors
%%  will be for expressions. Return the complete updated vartable including
%%  local variables and all updates. ShadowVarTable contains the state of
%%  each shadowed variable. All variable states of variables in ImportVarTable
%%  that have been shadowed are included in ShadowVarTable. In addition, all
%%  shadowed variables that are not included in ImportVarTable are included
%%  in ShadowVarTable (these are local variables that are not global variables).

lc_quals(Qs, Vt0, St0) ->
    lc_quals(Qs, Vt0, [], St0).

lc_quals([{generate,Line,P,E} | Qs], Vt, Uvt, St0) ->
    {Evt,St1} = expr(E, Vt, St0),
    %% Forget variables local to E immediately.
    Vt1 = vtupdate(vtold(Evt, Vt), Vt),
    {_, St2} = check_unused_vars(Evt, Vt, St1),
    {Pvt,Bvt,St3} = pattern(P, Vt1, [], St2),
    %% Have to keep fresh variables separated from used variables somehow
    %% in order to handle for example X = foo(), [X || <<X:X>> <- bar()].
    %%                                1           2      2 1
    Vt2 = vtupdate(Bvt, Vt1),
    St4 = shadow_vars(Line, Pvt, Vt2, generate, St3),
    Svt = vtold(Vt2, vtold(Pvt, Vt2)),
    {_, St5} = check_old_unused_vars(Svt, Uvt, St4),
    NUvt = vtupdate(vtnew(Svt, Uvt), Uvt),
    Vt3 = vtupdate(Pvt, vtsubtract(Vt2, Pvt)),
    lc_quals(Qs, Vt3, NUvt, St5);
lc_quals([F|Qs], Vt, Uvt, St0) ->
    {Fvt,St1} = case is_guard_test(F, St0#lint.records) of
                    true -> guard_test(F, Vt, St0);
                    false -> expr(F, Vt, St0)
                end,
    lc_quals(Qs, vtupdate(Fvt, Vt), Uvt, St1);
lc_quals([], Vt, Uvt, St) ->
    {Vt, Uvt, St}.

%% fun_clauses(Clauses, ImportVarTable, State) ->
%%      {UsedVars, State}.
%%  Fun's cannot export any variables.

fun_clauses(Cs, Vt, St) ->
    foldl(fun (C, {Bvt, St0}) ->
                  {Cvt,St1} = fun_clause(C, Vt, St0),
                  {vtmerge(Cvt, Bvt),St1}
          end, {[],St}, Cs).

fun_clause({clause,Line,H,G,B}, Vt0, St0) ->
    {Hvt,Binvt,St1} = head(H, Vt0, [], St0), % No imported pattern variables
    Vt1 = vtupdate(Binvt, Vt0),
    St2 = shadow_vars(Line, Hvt, Vt1, 'fun', St1),
    Svt = vtold(Vt1, vtold(Hvt, Vt1)),
    Vt2 = vtupdate(Hvt, vtsubtract(Vt1, Svt)),
    {Gvt,St3} = guard(G, Vt2, St2),
    Vt3 = vtupdate(Gvt, Vt2),
    {Bvt,St4} = exprs(B, Vt3, St3),
    Cvt = vtupdate(Bvt, Vt3),
    %% Check new local variables.
    {_, St5} = check_unused_vars(Cvt, Vt0, St4),
    %% Check all shadowing variables.
    {_, St6} = check_old_unused_vars(Cvt, Svt, St5),
    Vt4 = vtmerge(Svt, vtsubtract(Cvt, Svt)),
    {vtold(Vt4, Vt0),St6}.

%% In the variable table we store information about variables.  The
%% information is a pair {State,Usage}, the variables state and usage.
%% A variable can be in the following states:
%%
%% bound                everything is normal
%% {export,From}        variable has been exported
%% {unsafe,In}          variable is unsafe
%%
%% The usage information has the following form:
%%
%% used         variable has been used
%% {unused,[<Number>]}  variable bound on line <Number> but not used.
%%
%% Report variable errors/warnings as soon as possible and then change
%% the state to ok. This simplifies the code and reports errors only
%% once. Having the usage information like this makes it easy too when
%% merging states.

%% For keeping track of which variables are bound, ordsets are used.
%% In order to be able to give warnings about unused variables, a
%% possible value is {bound, {unused, [Line]}}. The usual value when
%% a variable is used is {bound, used}. An exception occurs for variables
%% in the size position in a bin element in a pattern. Currently, such a
%% variable is never matched out, always used, and therefore it makes no
%% sense to warn for "variable imported in match". It is also the case
%% that such a variable is not allowed in the head of a fun.

%% For storing the variable table we use the orddict module.
%% We know an empty set is [].

%% pat_var(Variable, LineNo, VarTable, State) -> {UpdVarTable,State}
%%  A pattern variable has been found. Handle errors and warnings. Return
%%  all variables as bound so errors and warnings are only reported once.

pat_var(V, Line, Vt, St) ->
    case orddict:find(V, Vt) of
        {ok,{bound,_Usage}} -> {[{V,{bound,used}}],St};
        {ok,{{unsafe,In},_Usage}} ->
            {[{V,{bound,used}}],add_error(Line, {unsafe_var,V,In}, St)};
        {ok,{{export,From}, _Usage}} ->
            {[{V,{bound,used}}],
             %% As this is matching, exported vars are risky.
             add_warning(Line, {exported_var,V,From}, St)};
        error -> {[{V,{bound,{unused,[Line]}}}],St}
    end.

%% pat_binsize_var(Variable, LineNo, VarTable, State) ->
%%      {UpdVarTable,State'}
%%  A pattern variable has been found. Handle errors and warnings. Return
%%  all variables as bound so errors and warnings are only reported once.

pat_binsize_var(V, Line, Vt, St) ->
    case orddict:find(V, Vt) of
        {ok,{bound,_Used}} ->
            {[{V,{bound,used}}],St};
        {ok,{{unsafe,In},_Used}} ->
            {[{V,{bound,used}}],add_error(Line, {unsafe_var,V,In}, St)};
        {ok,{{export,From},_Used}} ->
            {[{V,{bound,used}}],
             %% As this is not matching, exported vars are probably safe.
             exported_var(Line, V, From, St)};
        error ->
            {[{V,{bound,used}}],add_error(Line, {unbound_var,V}, St)}
    end.

%% expr_var(Variable, LineNo, VarTable, State) ->
%%      {UpdVarTable,State}
%%  Check if a variable is defined, or if there is an error or warning
%%  connected to its usage. Return all variables as bound so errors
%%  and warnings are only reported once.  As this is not matching
%%  exported vars are probably safe, warn only if warn_export_vars is
%%  set.

expr_var(V, Line, Vt, St0) ->
    St1 = case orddict:find(V, Vt) of
              {ok,{bound, _Usage}} -> St0;
              {ok,{{unsafe,In}, _Usage}} ->
                  add_error(Line, {unsafe_var,V,In}, St0);
              {ok,{{export,From}, _Usage}} ->
                  exported_var(Line, V, From, St0);
              error ->
                  add_error(Line, {unbound_var,V}, St0)
          end,
    {[{V,{bound,used}}],St1}.

exported_var(Line, V, From, St0) ->
    case St0#lint.warn_export of
        true -> add_warning(Line, {exported_var,V,From}, St0);
        false -> St0
    end.

shadow_vars(Line, Vt, Vt0, In, St0) ->
    case St0#lint.warn_shadow of
        true ->
            Vs = vtnames(vtold(Vt, vt_no_unsafe(Vt0))),
            foldl(fun (V, St) ->
                          add_warning(Line, {shadowed_var,V,In}, St)
                  end,
                  St0, Vs);
        false -> St0
    end.

check_unused_vars(Vt, Vt0, St0) ->
    U = unused_vars(Vt, Vt0, St0),
    warn_unused_vars(U, Vt, St0).

check_old_unused_vars(Vt, Vt0, St0) ->
    U = unused_vars(vtold(Vt, Vt0), [], St0),
    warn_unused_vars(U, Vt, St0).

unused_vars(Vt, Vt0, _St0) ->
    U0 = orddict:filter(fun (V, {_State,{unused,_Ls}}) -> 
                                case atom_to_list(V) of
                                    [$_|_] -> false;
                                    _ -> true
                                end;
                            (_V, _How) -> false
                        end, Vt),
    vtnew(U0, Vt0). % Only new variables.

warn_unused_vars([], Vt, St0) ->
    {Vt,St0};
warn_unused_vars(U, Vt, St0) ->
    St1 = 
        if
            St0#lint.warn_unused == false -> St0;
            true -> foldl(fun ({V,{_,{unused,Ls}}}, St) ->
                                  foldl(fun (L, St2) -> 
                                               add_warning(L, {unused_var,V},
                                                           St2)
                                        end, St, Ls)
                          end, St0, U)
          end,
    %% Return all variables as bound so warnings are only reported once.
    UVt = map(fun ({V,{State,_}}) -> {V,{State,used}} end, U),
    {vtmerge(Vt, UVt), St1}.

%% vtupdate(UpdVarTable, VarTable) -> VarTable.
%%  Add the variables in the updated vartable to VarTable. The variables
%%  will be updated with their property in UpdVarTable. The state of
%%  the variables in UpdVarTable will be returned.

vtupdate(Uvt, Vt0) ->
    orddict:merge(fun (_V, {S,U1}, {_S,U2}) ->
                          {S, merge_used(U1, U2)}
                  end, Uvt, Vt0).

%% vtexport([Variable], From, VarTable) -> VarTable.
%% vtunsafe([Variable], From, VarTable) -> VarTable.
%%  Add the variables to VarTable either as exported from From or as unsafe.

vtexport(Vs, From, Vt0) ->
    vtupdate([{V,{{export,From},{unused,[]}}} || V <- Vs], Vt0).

vtunsafe(Vs, In, Vt0) ->
    vtupdate([{V,{{unsafe,In},{unused,[]}}} || V <- Vs], Vt0).

%% vtmerge(VarTable, VarTable) -> VarTable.
%%  Merge two variables tables generating a new vartable. Give priority to
%%  errors then warnings.

vtmerge(Vt1, Vt2) ->
    orddict:merge(fun (_V, {S1,U1}, {S2,U2}) ->
                          {merge_state(S1, S2),merge_used(U1, U2)}
                  end, Vt1, Vt2).

vtmerge(Vts) -> foldl(fun (Vt, Mvts) -> vtmerge(Vt, Mvts) end, [], Vts).

vtmerge_pat(Vt1, Vt2) ->
    orddict:merge(fun (_V, {S1,_Usage1}, {S2,_Usage2}) ->
                          {merge_state(S1, S2),used}
                  end, Vt1, Vt2).

merge_state({unsafe,_F1}=S1, _S2) -> S1;          %Take the error case
merge_state(_S1, {unsafe,_F2}=S2) -> S2;
merge_state(bound, S2) -> S2;                   %Take the warning
merge_state(S1, bound) -> S1;
merge_state({export,F1},{export,_F2}) ->        %Sanity check
    %% We want to report the outermost construct
    {export,F1}.

merge_used(used, _Usage2) -> used;
merge_used(_Usage1, used) -> used;
merge_used({unused,Ls1}, {unused,Ls2}) -> 
    {unused,ordsets:union(Ls1,Ls2)}.

%% vtnew(NewVarTable, OldVarTable) -> NewVarTable.
%%  Return all the truly new variables in NewVarTable.

vtnew(New, Old) ->
    orddict:filter(fun (V, _How) -> not orddict:is_key(V, Old) end, New).

%% vtsubtract(VarTable1, VarTable2) -> NewVarTable.
%%  Return all the variables in VarTable1 which don't occur in VarTable2.
%%  Same thing as vtnew, but a more intuitive name for some uses.
vtsubtract(New, Old) ->
    vtnew(New, Old).

%% vtold(NewVarTable, OldVarTable) -> OldVarTable.
%%  Return all the truly old variables in NewVarTable.

vtold(New, Old) ->
    orddict:filter(fun (V, _How) -> orddict:is_key(V, Old) end, New).

vtnames(Vt) -> [ V || {V,_How} <- Vt ].

vt_no_unsafe(Vt) -> [V || {_,{S,_}}=V <- Vt,
                          case S of
                              {unsafe,_} -> false;
                              _ -> true
                          end].

%% vunion(VarTable1, VarTable2) -> [VarName].
%% vunion([VarTable]) -> [VarName].
%% vintersection(VarTable1, VarTable2) -> [VarName].
%% vintersection([VarTable]) -> [VarName].
%%  Union/intersection of names of vars in VarTable.

-ifdef(NOTUSED).
vunion(Vs1, Vs2) -> ordsets:union(vtnames(Vs1), vtnames(Vs2)).

vunion(Vss) -> foldl(fun (Vs, Uvs) -> 
                             ordsets:union(vtnames(Vs), Uvs)
                     end, [], Vss).

vintersection(Vs1, Vs2) -> ordsets:intersection(vtnames(Vs1), vtnames(Vs2)).
-endif.

vintersection([Vs]) ->
    vtnames(Vs);                %Boundary conditions!!!
vintersection([Vs|Vss]) ->
    ordsets:intersection(vtnames(Vs), vintersection(Vss));
vintersection([]) ->
    [].

%% copy_expr(Expr, Line) -> Expr.
%%  Make a copy of Expr converting all line numbers to Line.

copy_expr({clauses,Cs}, Line) -> {clauses,copy_expr(Cs, Line)};
copy_expr({function,F,A}, _Line) -> {function,F,A};
copy_expr({Tag,_L}, Line) -> {Tag,Line};
copy_expr({Tag,_L,E1}, Line) ->
    {Tag,Line,copy_expr(E1, Line)};
copy_expr({Tag,_L,E1,E2}, Line) ->
    {Tag,Line,copy_expr(E1, Line),copy_expr(E2, Line)};
copy_expr({Tag,_L,E1,E2,E3}, Line) ->
    {Tag,Line,
     copy_expr(E1, Line),
     copy_expr(E2, Line),
     copy_expr(E3, Line)};
copy_expr({Tag,_L,E1,E2,E3,E4}, Line) ->
    {Tag,Line,
     copy_expr(E1, Line),
     copy_expr(E2, Line),
     copy_expr(E3, Line),
     copy_expr(E4, Line)};
copy_expr([H|T], Line) ->
    [copy_expr(H, Line)|copy_expr(T, Line)];
copy_expr([], _Line) -> [];
copy_expr(E, _Line) when constant(E) -> E.

%% Check a record_info call. We have already checked that it is not
%% shadowed by an import.

check_record_info_call(_Line,La,[{atom,Li,Info},{atom,_Ln,Name}],St) ->
    case member(Info, [fields,size]) of
        true -> exist_record(La, Name, St);
        false -> add_error(Li, illegal_record_info, St)
    end;
check_record_info_call(Line,_La,_As,St) ->
    add_error(Line, illegal_record_info, St).

has_wildcard_field([{record_field,_Lf,{var,_La,'_'},_Val}|_Fs]) -> true;
has_wildcard_field([_|Fs]) -> has_wildcard_field(Fs);
has_wildcard_field([]) -> false.
     
%% check_remote_function(Line, ModuleName, FuncName, [Arg], State) -> State.
%%  Perform checks on known remote calls.

check_remote_function(Line, M, F, As, St0) ->
    St = obsolete_function(Line, M, F, As, St0),
    format_function(Line, M, F, As, St).

%% obsolete_function(Line, ModName, FuncName, [Arg], State) -> State.
%%  Add warning for calls to obsolete functions.

obsolete_function(Line, M, F, As, St) ->
    Arity = length(As),
    case otp_internal:obsolete(M, F, Arity) of
        {true, Info} ->
            add_warning(Line, {obsolete, {M, F, Arity}, Info}, St);
        false -> St
    end.

%% keyword_warning(Line, Atom, State) -> State.
%%  Add warning for atoms that will be reserved keywords in the future.
%%  (Currently, no such keywords to warn for.)
keyword_warning(_Line, _A, St) -> St.

%% format_function(Line, ModName, FuncName, [Arg], State) -> State.
%%  Add warning for bad calls to io:fwrite/format functions.

format_function(Line, M, F, As, St) ->
    case is_format_function(M, F) of
        true ->
            case St#lint.warn_format of
                Lev when Lev > 0 ->
                    case check_format_1(As) of
                        {warn,Level,Fmt,Fas} when Level =< Lev ->
                            add_warning(Line, {format_error,{Fmt,Fas}}, St);
                        _ -> St
                    end;
                _Lev -> St
            end;
        false -> St
    end.

is_format_function(io, fwrite) -> true;
is_format_function(io, format) -> true;
is_format_function(io_lib, fwrite) -> true;
is_format_function(io_lib, format) -> true;
is_format_function(_M, _F) -> false.

%% check_format_1([Arg]) -> ok | {warn,Level,Format,[Arg]}.

check_format_1([Fmt]) ->
    check_format_1([Fmt,{nil,0}]);
check_format_1([Fmt,As]) ->
    check_format_2(Fmt, canonicalize_string(As));
check_format_1([_Dev,Fmt,As]) ->
    check_format_1([Fmt,As]);
check_format_1(_As) ->
    {warn,1,"format call with wrong number of arguments",[]}.

canonicalize_string({string,Line,Cs}) ->
    foldr(fun (C, T) -> {cons,Line,{integer,Line,C},T} end, {nil,Line}, Cs);
canonicalize_string(Term) ->
    Term.

%% check_format_2([Arg]) -> ok | {warn,Level,Format,[Arg]}.

check_format_2(Fmt, As) ->
    case Fmt of
        {string,_L,S} -> check_format_2a(S, As);
        {atom,_L,A} -> check_format_2a(atom_to_list(A), As);
        _ -> {warn,2,"format string not a textual constant",[]}
    end.

check_format_2a(Fmt, As) ->
    case args_list(As) of
        true -> check_format_3(Fmt, As);
        false -> {warn,1,"format arguments not a list",[]};
        maybe -> {warn,2,"format arguments perhaps not a list",[]}
    end.

%% check_format_3(FormatString, [Arg]) -> ok | {warn,Level,Format,[Arg]}.

check_format_3(Fmt, As) ->
    case check_format_string(Fmt) of
        {ok,Need} ->
            case args_length(As) of
                Len when length(Need) == Len -> ok;
                _Len -> {warn,1,"wrong number of arguments in format call",[]}
            end;
        {error,S} ->
            {warn,1,"format string invalid (~s)",[S]}
    end.

args_list({cons,_L,_H,T}) -> args_list(T);
%% Strange case: user has written something like [a | "bcd"]; pretend
%% we don't know:
args_list({string,_L,_Cs}) -> maybe;
args_list({nil,_L}) -> true;
args_list({atom,_,_}) -> false;
args_list({integer,_,_}) -> false;
args_list({float,_,_}) -> false;
args_list(_Other) -> maybe.

args_length({cons,_L,_H,T}) -> 1 + args_length(T);
args_length({nil,_L}) -> 0.

check_format_string(Fmt) ->
    extract_sequences(Fmt, []).

extract_sequences(Fmt, Need0) ->
    case string:chr(Fmt, $~) of
        0 -> {ok,lists:reverse(Need0)};         %That's it
        Pos ->
            Fmt1 = string:substr(Fmt, Pos+1),   %Skip ~
            case extract_sequence(1, Fmt1, Need0) of
                {ok,Need1,Rest} -> extract_sequences(Rest, Need1);
                Error -> Error
            end
    end.

extract_sequence(1, [$-,C|Fmt], Need) when C >= $0, C =< $9 ->
    extract_sequence_digits(1, Fmt, Need);
extract_sequence(1, [C|Fmt], Need) when C >= $0, C =< $9 ->
    extract_sequence_digits(1, Fmt, Need);
extract_sequence(1, [$-,$*|Fmt], Need) ->
    extract_sequence(2, Fmt, [int|Need]);
extract_sequence(1, [$*|Fmt], Need) ->
    extract_sequence(2, Fmt, [int|Need]);
extract_sequence(1, Fmt, Need) ->
    extract_sequence(2, Fmt, Need);
extract_sequence(2, [$.,C|Fmt], Need) when C >= $0, C =< $9 ->
    extract_sequence_digits(2, Fmt, Need);
extract_sequence(2, [$.,$*|Fmt], Need) ->
    extract_sequence(3, Fmt, [int|Need]);
extract_sequence(2, [$.|Fmt], Need) ->
    extract_sequence(3, Fmt, Need);
extract_sequence(2, Fmt, Need) ->
    extract_sequence(4, Fmt, Need);
extract_sequence(3, [$.,$*|Fmt], Need) ->
    extract_sequence(4, Fmt, [int|Need]);
extract_sequence(3, [$.,_|Fmt], Need) ->
    extract_sequence(4, Fmt, Need);
extract_sequence(3, Fmt, Need) ->
    extract_sequence(4, Fmt, Need);
extract_sequence(4, [C|Fmt], Need0) ->
    case control_type(C, Need0) of
        error -> {error,"invalid control ~" ++ [C]};
        Need1 -> {ok,Need1,Fmt}
    end;
extract_sequence(_, [], _Need) -> {error,"truncated"}.

extract_sequence_digits(Fld, [C|Fmt], Need) when C >= $0, C =< $9 ->
    extract_sequence_digits(Fld, Fmt, Need);
extract_sequence_digits(Fld, Fmt, Need) ->
    extract_sequence(Fld+1, Fmt, Need).

control_type($~, Need) -> Need;
control_type($c, Need) -> [int|Need];
control_type($f, Need) -> [float|Need];
control_type($e, Need) -> [float|Need];
control_type($g, Need) -> [float|Need];
control_type($s, Need) -> [string|Need];
control_type($w, Need) -> [term|Need];
control_type($p, Need) -> [term|Need];
control_type($W, Need) -> [int,term|Need]; %% Note: reversed
control_type($P, Need) -> [int,term|Need]; %% Note: reversed
control_type($b, Need) -> [term|Need];
control_type($B, Need) -> [term|Need];
control_type($x, Need) -> [string,term|Need]; %% Note: reversed
control_type($X, Need) -> [string,term|Need]; %% Note: reversed
control_type($+, Need) -> [term|Need];
control_type($#, Need) -> [term|Need];
control_type($n, Need) -> Need;
control_type($i, Need) -> [term|Need];
control_type(_C, _Need) -> error.

%% In syntax trees, module/package names are atoms or lists of atoms.

package_to_string(A) when atom(A) -> atom_to_list(A);
package_to_string(L) when list(L) -> packages:concat(L).

expand_package({atom,L,A} = M, St0) ->
    St1 = keyword_warning(L, A, St0),
    case dict:find(A, St1#lint.mod_imports) of
        {ok, A1} ->
            {{atom,L,A1}, St1};
        error ->
            Name = atom_to_list(A),
            case packages:is_valid(Name) of
                true ->
                    case packages:is_segmented(Name) of
                        true ->
                            {M, St1};
                        false ->
                            M1 = packages:concat(St1#lint.package,
                                                 Name),
                            {{atom,L,list_to_atom(M1)}, St1}
                    end;
                false ->
                    St2 = add_error(L, {bad_module_name, Name}, St1),
                    {error, St2}
            end
    end;
expand_package(M, St0) ->
    L = element(2, M),
    case erl_parse:package_segments(M) of
        error ->
            {error, St0};
        M1 ->
            Name = package_to_string(M1),
            case packages:is_valid(Name) of
                true ->
                    {{atom,L,list_to_atom(Name)}, St0};
                false ->
                    St1 = add_error(L, {bad_module_name, Name}, St0),
                    {error, St1}
            end
    end.
