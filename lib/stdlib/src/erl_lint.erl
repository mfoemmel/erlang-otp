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
%% Do necessary checking of Erlang code.

%% N.B. All the code necessary for checking structs (tagged tuples) is
%% here. Just comment out the lines in pattern/2, gexpr/3 and expr/3.

-module(erl_lint).

-export([module/1,module/2,module/3,format_error/1]).
-export([is_guard_test/1,is_guard_expr/1]).

-import(lists, [member/2,map/2,foldl/3,reverse/1,filter/2,all/2]).
-import(ordsets, [list_to_set/1,add_element/2,set_to_list/1,
		  union/2,intersection/2,subtract/2]).

%% The error and warning info structures, {Line,Module,Descriptor}, are
%% kept in reverse order in their seperate fields in the lint state record.
%% When a new file is entered, marked by the file attribute then a
%% {file,FileName} pair is pushed on each list. At the end of the run these
%% lists are packed, and reversed, into a list of {FileName,ErrorDescList}
%% pairs which are returned.

%% Define the lint state record.
%% 'called' and 'exports' contain {Line, {Function, Arity}},
%% the other function collections contain {Function, Arity}.
%% 'called' is a list, not an ordset.
-record(lint, {state=start,			%start | attribute | function
	       module=[],			%Module
	       behaviour=[],                    %Behaviour
	       exports=[],			%Exports
	       imports=[],			%Imports
	       compile=[],			%Compile flags
	       records=dict:new(),		%Record definitions
	       defined=[],			%Defined fuctions
	       called=[],			%Called functions
	       imported=[],			%Actually imported functions
	       func=[],				%Current function
	       errors=[],			%Current errors
	       warnings=[]			%Current warnings
	      }).

%% format_error(Error)
%%  Return a string describing the error.

format_error(undefined_module) ->
    "no module definition";
format_error(redefine_module) ->
    "redefining module";

format_error(invalid_call) ->
    "invalid function call";
format_error(invalid_record) ->
    "invalid record expression";

format_error({attribute,A}) ->
    io_lib:format("attribute '~w' after function definitions", [A]);
format_error({redefine_import,{{F,A},M}}) ->
    io_lib:format("function ~w/~w already imported from ~w", [F,A,M]);

format_error(export_all) ->
    "non-recommended option 'export_all' used";

format_error({format, {Fmt, Args}}) ->
    io_lib:format("~s", [io_lib:format(Fmt, Args)]);

format_error({unused_var, V}) ->
    io_lib:format("unused variable ~s", [V]);

format_error({unused_import,{{F,A},M}}) ->
    io_lib:format("import ~w:~w/~w not called", [M,F,A]);
format_error({undefined_function,{F,A}}) ->
    io_lib:format("function ~w/~w undefined", [F,A]);
format_error({redefine_function,{F,A}}) ->
    io_lib:format("function ~w/~w already defined", [F,A]);
format_error({import_and_define,{F,A}}) ->
    io_lib:format("defining already imported function ~w/~w", [F,A]);
format_error({not_called,{F,A}}) ->
    io_lib:format("function ~w/~w not called", [F,A]);
format_error({redefine_bif,{F,A}}) ->
    io_lib:format("defining BIF ~w/~w", [F,A]);
format_error(asm) -> "illegal asm";

format_error({obsolete, {M1, F1, A1}, {M2, F2, A2}}) ->
    io_lib:format("~p:~p/~p obsolete; use ~p:~p/~p", [M1, F1, A1, M2, F2, A2]);
format_error({obsolete, {M1, F1, A1}, String}) when list(String) ->
    io_lib:format("~p:~p/~p: ~s", [M1, F1, A1, String]);

format_error(pattern) -> "illegal pattern";
format_error(expr) -> "illegal expression";
format_error(guard_expr) -> "illegal guard expression";

format_error({undefined_record,T}) ->
    io_lib:format("record ~w undefined", [T]);
format_error({redefine_record,T}) ->
    io_lib:format("record ~w already defined", [T]);
format_error({redefine_field,T,F}) ->
    io_lib:format("field ~w already defined in record ~w", [F,T]);
format_error({undefined_field,T,F}) ->
    io_lib:format("field ~w undefined in record ~w", [F,T]);
format_error(record_info) ->
    "illegal record info";

format_error({unbound_var,V}) ->
    io_lib:format("variable ~w is unbound", [V]);
format_error({unsafe_var,V}) ->
    io_lib:format("variable ~w is unsafe", [V]);
format_error({exported_var,V,From}) ->
    io_lib:format("variable ~w exported from ~w", [V,From]);
format_error({imported_var,V,In}) ->
    io_lib:format("variable ~w imported in ~w", [V,In]);
format_error({shadowed_var,V,In}) ->
    io_lib:format("variable ~w shadowed in ~w", [V,In]);

format_error({mnemosyne,What}) ->
    "mnemosyne " ++ What ++ ", missing transformation";
format_error({undefined_behaviour,Behaviour}) ->
    io_lib:format("behaviour ~w undefined", [Behaviour]);
format_error({several_behaviours,Behaviours}) ->
    io_lib:format("several behaviours defined - ~p", [Behaviours]);
format_error({undefined_behaviour_func, {Func, Arity}}) ->
    io_lib:format("undefined call-back function ~w/~w", [Func, Arity]).

%% module([Form]) ->
%% module([Form], FileName) ->
%% module([Form], FileName, [CompileOption]) ->
%%	{ok,[Warning]} | {error,[Error],[Warning]}
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
    #lint{exports=add_l(0,[{module_info,0},
			   {module_info,1}]),
	  compile=Opts,
	  defined=lists:sort([{module_info,0},
			      {module_info,1},
			      {record_info,2}]),
	  called=add_l(0,[{record_info,2}]),
	  errors=[{file,File}],
	  warnings=[{file,File}]}.

%% return_status(State) ->
%%	{ok,[Warning]} | {error,[Error],[Warning]}
%%  Pack errors and warnings properly and return ok | error.

return_status(St) ->
    Ws = pack_errors(St#lint.warnings, [], []),
    case pack_errors(St#lint.errors, [], []) of
	[] -> {ok,Ws};
	Es -> {error,Es,Ws}
    end.

%% pack_errors([ErrD], [ErrD], [FileErrD]) -> [FileErrD].
%%  We know that the errors have been inserted in reverse order.

pack_errors([{file,F}|Es], [], Ps) ->
    pack_errors(Es, [], Ps);
pack_errors([{file,F}|Es], Fes, Ps) ->
    pack_errors(Es, [], [{F,Fes}|Ps]);
pack_errors([E|Es], Fes, Ps) ->
    pack_errors(Es, [E|Fes], Ps);
pack_errors([], Fes, Ps) -> Ps. 

%% add_error(ErrorDescriptor, State) -> State'
%% add_error(Line, Error, State) -> State'
%% add_warning(ErrorDescriptor, State) -> State'
%% add_warning(Line, Error, State) -> State'

add_error(E, St) -> St#lint{errors=[E|St#lint.errors]}.
add_error(Line, E, St) -> add_error({Line,erl_lint,E}, St).

add_warning(W, St) -> St#lint{warnings=[W|St#lint.warnings]}.
add_warning(Line, W, St) -> add_warning({Line,erl_lint,W}, St).

%% forms([Form], State) -> State'

forms(Forms, St) ->
    foldl(fun form/2, St, Forms).

%% form(Form, State) -> State'
%%  Check a form returning the updated State. Handle generic cases here.

form({error,E}, St)   -> add_error(E, St);
form({warning,W}, St) -> add_warning(W, St);
form({attribute,L,file,{File,Line}}, St) ->
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
    St#lint{state=attribute,module=M};
start_state(Form, St0) ->
    St1 = add_error(element(2, Form), undefined_module, St0),
    attribute_state(Form, St1#lint{state=attribute}).

%% attribute_state(Form, State) ->
%%	State'

add_l(L, List) ->
    lists:sort(lists:map(fun(X) -> {L,X} end, List)).

attribute_state({attribute,L,module,M}, St) ->
    add_error(L, redefine_module, St);
attribute_state({attribute,L,export,Es}, St) ->
    St#lint{exports=union(list_to_set(add_l(L,Es)), St#lint.exports)};
attribute_state({attribute,L,import,Is}, St) ->
    import(L, Is, St);
attribute_state({attribute,L,record,{Name,Fields}}, St) ->
    record_def(L, Name, Fields, St);
attribute_state({attribute,La,compile,C}, St) when list(C) ->
    St#lint{compile=St#lint.compile ++ C};
attribute_state({attribute,La,compile,C}, St) ->
    St#lint{compile=St#lint.compile ++ [C]};
attribute_state({attribute,La,asm,Func}, St) ->
    asm(La, Func, St);
attribute_state({attribute,La,behaviour,Behaviour}, St) ->
    St#lint{behaviour=[{La,Behaviour}|St#lint.behaviour]};
attribute_state({attribute,La,behavior,Behaviour}, St) ->
    St#lint{behaviour=[{La,Behaviour}|St#lint.behaviour]};
attribute_state({attribute,L,Other,Val}, St) ->	%Ignore others
    St;
attribute_state(Form, St) ->
    function_state(Form, St#lint{state=function}).

%% function_state(Form, State) ->
%%	State'
%%  Allow record definitions here!

function_state({attribute,L,record,{Name,Fields}}, St) ->
    record_def(L, Name, Fields, St);
function_state({attribute,La,asm,Func}, St) ->
    asm(La, Func, St);
function_state({attribute,La,Attr,Val}, St) ->
    add_error(La, {attribute,Attr}, St);
function_state({function,L,N,A,Cs}, St) ->
    function(L, N, A, Cs, St);
function_state({rule,L,N,A,Cs}, St) ->
    add_error(L, {mnemosyne,"rule"}, St);
function_state({eof,L}, St) -> eof(L, St).

%% eof(LastLine, State) ->
%%	State'

%%% Removes duplicates from a sorted list.
remove_dup([E | Rest]) ->
    remove_dup(E, Rest);
remove_dup([]) ->
    [].

remove_dup(E, [E | Rest]) ->
    remove_dup(E, Rest);
remove_dup(E, [X | Rest]) ->
    [E | remove_dup(X, Rest)];
remove_dup(E, []) ->
    [E].

eof(Line, St0) ->
    %% Check that the behaviour attribute is valid.
    St1 = behaviour_check(St0#lint.behaviour, St0),
    Referred0 = union(St1#lint.exports,
		      list_to_set(remove_dup(lists:sort(St1#lint.called)))),
    St2 = case lists:member(export_all, St0#lint.compile) of
	      true ->
		  add_warning(Line, export_all, St1);
	      false ->
		  %% Get all functions that have been called or referred to.
		  Referred_L = lists:map(fun({_,X}) -> X end,
					 set_to_list(Referred0)),
		  %% (We happen to know that list_to_set does an insertion
		  %% sort; if we sort ourselves first, it goes faster.)
		  Referred = list_to_set(lists:sort(Referred_L)),
		  %% Generate warnings.
		  func_warning(Line, not_called,
			       subtract(St1#lint.defined, Referred), St1)
	  end,
    St3 = func_warning(Line, unused_import,
		       subtract(St2#lint.imports, St2#lint.imported), St2),
    %% Generate errors.
    func_error_l(undefined_function,
		 subtract_l(Referred0, St2#lint.defined), St3).

subtract_l([], _) ->
    [];
subtract_l([{Line, F} | Rest], Subtractor) ->
    case lists:member(F, Subtractor) of
	true ->
	    subtract_l(Rest, Subtractor);
	false ->
	    [{Line, F} | subtract_l(Rest, Subtractor)]
    end.

func_error_l(Type, Fs, St) ->
    foldl(fun ({Line,F}, St0) -> add_error(Line, {Type,F}, St0) end, St, Fs).

func_warning(Line, Type, Fs, St) ->
    foldl(fun (F, St0) -> add_warning(Line, {Type,F}, St0) end, St, Fs).

%% behaviour_check([{Line,Behaviour}], State) -> State'

behaviour_check([], St) ->
    St;
behaviour_check(Behaviours, St) when length(Behaviours) == 1 ->
    behaviour_check1(Behaviours, St);
behaviour_check(Behaviours0, St) ->
    Behaviours = reverse(Behaviours0),
    [{Line,_}|_] = Behaviours,
    Bs = map(fun({_,B}) -> B end, Behaviours),
    St1 = func_warning(Line, several_behaviours, [Bs], St),
    behaviour_check1(Behaviours, St1).

behaviour_check1([{Line,Behaviour}|Bhs], St0) ->
    St = case member(Behaviour, otp_internal:behaviour_info()) of
	     true ->
		 Exps = otp_internal:behaviour_info(Behaviour),

		 %% (See comment in eof/2.)
		 Exported_L = lists:map(fun({_,X}) -> X end,
					St0#lint.exports),
		 Exported = list_to_set(lists:sort(Exported_L)),
		 Missing = subtract(list_to_set(Exps),
				    Exported),
		 func_warning(Line, undefined_behaviour_func,
			      Missing, St0);
	     _ ->
		 func_warning(Line, undefined_behaviour, [Behaviour], St0)
	 end,
    behaviour_check1(Bhs, St);
behaviour_check1([], St) ->
    St.

%% asm(Line, Function, State) -> State'

asm(Line, {function,Name,Arity,Code}, St) ->
    define_function(Line, Name, Arity, St);
asm(Line, Func, St) ->
    add_error(Line, asm, St).

%% For storing the import list we use our own version of the module dict.
%% This is/was the same as the original but we must be sure of the format
%% (sorted list of pairs) so we can do ordset operations on them (see the
%% the function eof/2. We know an empty set is [].

%% import(Line, Imports, State) -> State.
%% imported(Name, Arity, State) -> {yes,Module} | no.

import(Line, {Mod,Fs}, St) ->
    Mfs = list_to_set(Fs),
    case check_imports(Line, Mfs, St#lint.imports) of
	[] ->
	    St#lint{imports=add_imports(Mod, Mfs, St#lint.imports)};
	Efs ->
	    foldl(fun (Ef, St0) ->
		      add_error(Line, {redefine_import,Ef}, St0) end,
		  St, Efs)
    end.

check_imports(Line, Fs, Is) ->
    foldl(fun (F, Efs) ->
	      case find(F, Is) of
		  {ok,Mod} -> [{F,Mod}|Efs];
		  error -> Efs
	      end end, [], Fs).

add_imports(Mod, Fs, Is) ->
    foldl(fun (F, Is0) -> store(F, Mod, Is0) end, Is, Fs).

imported(F, A, St) ->
    case find({F,A}, St#lint.imports) of
	{ok,Mod} -> {yes,Mod};
	error -> no
    end.

%% function(Line, Name, Arity, Clauses, State) -> State.

function(Line, Name, Arity, Cs, St0) ->
    St1 = define_function(Line, Name, Arity, St0#lint{func={Name,Arity}}),
    clauses(Cs, St1).

%% define_function(Line, Name, Arity, State) -> State.

define_function(Line, Name, Arity, St0) ->
    NA = {Name,Arity},
    case member(NA, St0#lint.defined) of
	true ->
	    add_error(Line, {redefine_function,NA}, St0);
	false ->
	    St1 = St0#lint{defined=add_element(NA, St0#lint.defined)},
	    St2 = case erl_internal:bif(Name, Arity) of
		      true ->
			  add_warning(Line, {redefine_bif,NA}, St1);
		      false -> St1
		  end,
	    case imported(Name, Arity, St2) of
		{yes, M} ->
		    add_error(Line, {import_and_define,NA}, St2);
		no ->
		    St2
	    end
    end.

%% clauses([Clause], State) -> State.

report_unused([], St) ->
    St;
report_unused([{V, {_, {unused, Line}}} | Rest], St0) ->
    St1 = case lists:member(warn_unused_vars, St0#lint.compile) of
	      true ->
		  case atom_to_list(V) of
		      [$_ | _] ->
			  St0;
		      _ ->
			  add_warning(Line, {unused_var, V}, St0)
		  end;
	      false ->
		  St0
	  end,
%    io:format("Variable ~w (~p) unused~n", [V, Line]),
    report_unused(Rest, St1);
report_unused([_|Rest], St) ->
    report_unused(Rest, St).

clauses([{clause,Line,H,G,B}|Cs], St0) ->
    {Hvt,St1} = head(H, [], St0),
    {Gvt,St2} = guard(G, Hvt, St1),
    {Evt,St3} = exprs(B, X=vtupdate(Gvt, Hvt), St2),
%    io:format("c ~p~n", [{hvt, Hvt, gvt, Gvt, x, X, evt, Evt}]),
    St4 = report_unused(vtupdate(Evt, X), St3),
    clauses(Cs, St4);
clauses([], St) -> St.

%% head([HeadPattern], VarTable, State) ->
%%	{VarTable,State}
%%  Check a patterns in head returning "all" variables. Not updating the
%%  known variable list will result in multiple error messages/warnings.

head([P|Ps], Vt, St0) ->
    {Pvt,St1} = pattern(P, Vt, St0),
    {Psvt,St2} = head(Ps, Vt, St1),
    {vtmerge_pat(Pvt, Psvt),St2};
head([], Vt, St) -> {[],St}.

%% pattern(Pattern, VarTable, State) -> {UpdVarTable,State}.
%%  Check pattern return variables.

string_to_conses([], Line, Tail) ->
    Tail;
string_to_conses([E|Rest], Line, Tail) ->
    {cons, Line, {integer, Line, E}, string_to_conses(Rest, Line, Tail)}.

pattern({var,Line,'_'}, Vt, St) -> {[],St};	%Ignore anonymous variable
pattern({match,Line,Pat1,Pat2}, Vt, St0) ->
    {Lvt,St1} = pattern(Pat1, Vt, St0),
    {Rvt,St2} = pattern(Pat2, Vt, St1),
    {vtmerge_pat(Lvt, Rvt),St2};
pattern({var,Line,V}, Vt, St) -> pat_var(V, Line, Vt, St);
pattern({integer,Line,I}, Vt, St) -> {[],St};
pattern({float,Line,F}, Vt, St) -> {[],St};
pattern({atom,Line,A}, Vt, St) -> {[],St};
pattern({string,Line,S}, Vt, St) -> {[],St};
pattern({nil,Line}, Vt, St) -> {[],St};
pattern({cons,Line,H,T}, Vt,  St0) ->
    {Hvt,St1} = pattern(H, Vt, St0),
    {Tvt,St2} = pattern(T, Vt, St1),
    {vtmerge_pat(Hvt, Tvt),St2};
pattern({tuple,Line,Ps}, Vt, St) ->
    pattern_list(Ps, Vt, St);
%%pattern({struct,Line,Tag,Ps}, St) ->
%%    pattern_list(Ps, Vt, St);
pattern({record_index,Line,Name,Field}, Vt, St) ->
    check_record(Line, Name, St,
		 fun (Dfs) -> pattern_field(Field, Name, Dfs, Vt, St) end);
pattern({record,Line,Name,Pfs}, Vt, St) ->
    check_record(Line, Name, St,
		 fun (Dfs) -> pattern_fields(Pfs, Name, Dfs, Vt, St) end);
pattern({op,Line,'++',{nil,_},R}, Vt, St) ->
    pattern(R, Vt, St);
pattern({op,Line,'++',{cons,Li,{integer,L2,I},T},R}, Vt, St) ->
    pattern({cons,Li,{integer,L2,I},{op,Li,'++',T,R}}, Vt, St);
pattern({op,Line,'++',{string,Li,L},R}, Vt, St) ->
    pattern(string_to_conses(L, Li, R), Vt, St);
%% The following are necessary to be able to handle unary +,- in patterns.
pattern({op,Line,'+',{integer,Li,I}}, Vt, St) -> {[],St};
pattern({op,Line,'-',{integer,Li,I}}, Vt, St) -> {[],St};
pattern({op,Line,'+',{float,Lf,F}}, Vt, St) -> {[],St};
pattern({op,Line,'-',{float,Lf,F}}, Vt, St) -> {[],St};
pattern(Other, Vt, St) ->
    {[],add_error(element(2, Other), pattern, St)}.

pattern_list(Ps, Vt, St) ->
    foldl(fun (P, {Psvt,St0}) ->
		  {Pvt,St1} = pattern(P, Vt, St0),
		  {vtmerge_pat(Pvt, Psvt),St1}
	  end, {[],St}, Ps).

%% guard([GuardTest], VarTable, State) ->
%%	{UsedVarTable,State}
%%  Check a guard, return all variables.

%% disjunction of guard conjunctions
guard([L|R], Vt, St0) when list(L) ->
    {Gvt, St1} = guard0(L, Vt, St0),
    {Gsvt, St2} = guard(R, vtupdate(Gvt, Vt), St1),
    {vtupdate(Gvt, Gsvt),St2};
guard(L, Vt, St0) ->
    guard0(L, Vt, St0).

%% guard conjunction
guard0([G|Gs], Vt, St0) ->
    {Gvt,St1} = guard_test(G, Vt, St0),
    {Gsvt,St2} = guard0(Gs, vtupdate(Gvt, Vt), St1),
    {vtupdate(Gvt, Gsvt),St2};
guard0([], Vt, St) -> {[],St}.

%% guard_test(Test, VarTable, State) ->
%%	{UsedVarTable,State'}
%%  Check one guard test, returns NewVariables

%% These are special for now.
guard_test({atom,Line,true}, Vt, St) -> {[], St};
%% Specially handle record type test here.
guard_test({call,Line,{atom,Lr,record},[E,{atom,Ln,Name}]}, Vt, St0) ->
    {Rvt,St1} = gexpr(E, Vt, St0),
    {Rvt,exist_record(Ln, Name, St1)};
guard_test({call,Line,{atom,Lr,record},[E,R]}, Vt, St) ->
    {[],add_error(Line, guard_expr, St)};
guard_test({call,Line,{atom,La,F},As}, Vt, St0) ->
    {Asvt,St1} = gexpr_list(As, Vt, St0),
    A = length(As),
    case erl_internal:type_test(F, A) of
	true -> {Asvt,St1};
	false -> {Asvt,add_error(Line, guard_expr, St1)}
    end;
guard_test({op,Line,Op,L,R}, Vt, St0) ->
    {Avt,St1} = gexpr_list([L,R], Vt, St0),
    case erl_internal:comp_op(Op, 2) of
	true -> {Avt,St1};
	false -> {Avt,add_error(Line, guard_expr, St1)}
    end;
%% Everything else is illegal! You could put explicit tests here to get
%% better error diagnostics.
guard_test(G, Vt, St) ->
    {[],add_error(element(2, G), guard_expr, St)}.

%% gexpr(GuardExpression, VarTable, State) ->
%%      {UsedVarTable,State'}
%%  Check a guard expression, returns NewVariables.

gexpr({var,Line,V}, Vt, St) ->
    expr_var(V, Line, Vt, St);
gexpr({integer,Line,I}, Vt, St) -> {[],St};
gexpr({float,Line,F}, Vt, St) -> {[],St};
gexpr({atom,Line,A}, Vt, St) -> {[],St};
gexpr({string,Line,S}, Vt, St) -> {[],St};
gexpr({nil,Line}, Vt, St) -> {[],St};
gexpr({cons,Line,H,T}, Vt, St) ->
    gexpr_list([H,T], Vt, St);
gexpr({tuple,Line,Es}, Vt, St) ->
    gexpr_list(Es, Vt, St);
%%gexpr({struct,Line,Tag,Es}, Vt, St) ->
%%    gexpr_list(Es, Vt, St);
gexpr({record_index,Line,Name,Field}, Vt, St) ->
    check_record(Line, Name, St,
		 fun (Dfs) -> record_field(Field, Name, Dfs, Vt, St) end );
gexpr({record_field,Line,Rec,Name,Field}, Vt, St0) ->
    {Rvt,St1} = gexpr(Rec, Vt, St0),
    {Fvt,St2} = check_record(Line, Name, St1,
			     fun (Dfs) ->
				     record_field(Field, Name, Dfs, Vt, St1)
			     end),
    {vtmerge(Rvt, Fvt),St2};
gexpr({record,Line,Name,Inits}, Vt, St) ->
    check_record(Line, Name, St,
		 fun (Dfs) ->
			 ginit_fields(Inits, Line, Name, Dfs, Vt, St)
		 end);
gexpr({call,Line,{atom,La,F},As}, Vt, St0) ->
    {Asvt,St1} = gexpr_list(As, Vt, St0),
    A = length(As),
    case erl_internal:guard_bif(F, A) of
	true -> {Asvt,St1};
	false -> {Asvt,add_error(Line, guard_expr, St1)}
    end;
gexpr({op,Line,Op,A}, Vt, St0) ->
    {Avt,St1} = gexpr(A, Vt, St0),
    case erl_internal:arith_op(Op, 1) of
	true -> {Avt,St1};
	false -> {Avt,add_error(Line, guard_expr, St1)}
    end;
gexpr({op,Line,Op,L,R}, Vt, St0) ->
    {Avt,St1} = gexpr_list([L,R], Vt, St0),
    case erl_internal:arith_op(Op, 2) of
	true -> {Avt,St1};
	false -> {Avt,add_error(Line, guard_expr, St1)}
    end;
%% Everything else is illegal! You could put explicit tests here to
%% better error diagnostics.
gexpr(E, Vt, St) ->
    {[],add_error(element(2, E), guard_expr, St)}.

%% gexpr_list(Expressions, VarTable, State) ->
%%      {UsedVarTable,State'}

gexpr_list(Es, Vt, St) ->
    foldl(fun (E, {Esvt,St0}) ->
		  {Evt,St1} = gexpr(E, Vt, St0),
		  {vtmerge(Evt, Esvt),St1}
	  end, {[],St}, Es).

%% is_guard_test(Expression) ->
%%	true | false
%%  Test if a general expression is a guard test.

is_guard_test({op,Line,Op,L,R}) ->
    %% all_of erl_internal:comp_op(Op, 2), is_gexpr(L), is_gexpr(R) end.
    case erl_internal:comp_op(Op, 2) of
	true -> is_gexpr_list([L,R]);
	false -> false
    end;
is_guard_test({call,Line,{atom,La,Test},As}) ->
    case erl_internal:type_test(Test, length(As)) of
	true -> is_gexpr_list(As);
	false -> false
    end;
is_guard_test({atom,Line,true}) -> true;
is_guard_test(Other) -> false.

%% is_guard_expr(Expression) -> true | false.
%%  Test if an expression is a guard expression.

is_guard_expr(E) -> is_gexpr(E). 

is_gexpr({var,L,V}) -> true;
is_gexpr({atom,L,A}) -> true;
is_gexpr({integer,L,I}) -> true;
is_gexpr({float,L,F}) -> true;
is_gexpr({string,L,S}) -> true;
is_gexpr({nil,L}) -> true;
is_gexpr({cons,L,H,T}) -> is_gexpr_list([H,T]);
is_gexpr({tuple,L,Es}) -> is_gexpr_list(Es);
%%is_gexpr({struct,L,Tag,Es}) ->
%%    is_gexpr_list(Es);
is_gexpr({record_index,L,Name,Field}) ->
    is_gexpr(Field);
is_gexpr({record_field,L,Rec,Name,Field}) ->
    is_gexpr_list([Rec,Field]);
is_gexpr({record,L,Name,Inits}) ->
    is_gexpr_fields(Inits);
is_gexpr({call,L,{atom,La,F},As}) ->
    case erl_internal:guard_bif(F, length(As)) of
	true -> is_gexpr_list(As);
	false -> false
    end;
is_gexpr({op,L,Op,A}) ->
    case erl_internal:arith_op(Op, 1) of
	true -> is_gexpr(A);
	false -> false
    end;
is_gexpr({op,L,Op,A1,A2}) ->
    case erl_internal:arith_op(Op, 2) of
	true -> is_gexpr_list([A1,A2]);
	false -> false
    end;
is_gexpr(Other) -> false.

is_gexpr_list(Es) -> all(fun (E) -> is_gexpr(E) end, Es).

is_gexpr_fields(Fs) ->
    all(fun ({record_field,Lf,F,V}) -> is_gexpr(V);
	    (Other) -> false end, Fs).

%% exprs(Sequence, VarTable, State) ->
%%	{UsedVarTable,State'}
%%  Check a sequence of expressions, return all variables.

exprs([E|Es], Vt, St0) ->
    {Evt,St1} = expr(E, Vt, St0),
    {Esvt,St2} = exprs(Es, vtupdate(Evt, Vt), St1),
%    io:format("e ~p~n", [{vt, Vt, evt, Evt, esvt, Esvt, up,
%			  vtupdate(Evt, Esvt)}]),
    {vtupdate(Evt, Esvt),St2};
exprs([], Vt, St) -> {[],St}.

%% expr(Expression, VarTable, State) ->
%%      {UsedVarTable,State'}
%%  Check an expression, returns NewVariables. Assume naive users and
%%  mark illegally exported variables, e.g. from catch, as unsafe to better
%%  show why unbound.

expr({var,Line,V}, Vt, St) ->
    expr_var(V, Line, Vt, St);
expr({integer,Line,I}, Vt, St) -> {[],St};
expr({float,Line,F}, Vt, St) -> {[],St};
expr({atom,Line,A}, Vt, St) -> {[],St};
expr({string,Line,S}, Vt, St) -> {[],St};
expr({nil,Line}, Vt, St) -> {[],St};
expr({cons,Line,H,T}, Vt, St) ->
    expr_list([H,T], Vt, St);
expr({lc,Line,E,Qs}, Vt0, St0) ->
    %% No new variables added.
    {Qvt,St1} = lc_quals(Qs, Vt0, St0),		%Qvt is used variables
    {Evt,St2} = expr(E, vtupdate(Qvt, Vt0), St1),
    {[],St2};					%Export nothing!
expr({tuple,Line,Es}, Vt, St) ->
    expr_list(Es, Vt, St);
%%expr({struct,Line,Tag,Es}, Vt, St) ->
%%    expr_list(Es, Vt, St);
expr({record_index,Line,Name,Field}, Vt, St) ->
    check_record(Line, Name, St,
		 fun (Dfs) -> record_field(Field, Name, Dfs, Vt, St) end);
expr({record,Line,Name,Inits}, Vt, St) ->
    check_record(Line, Name, St,
		 fun (Dfs) -> init_fields(Inits, Line, Name, Dfs, Vt, St) end);
expr({record_field,Line,Rec,Name,Field}, Vt, St0) ->
    {Rvt,St1} = record_expr(Line, Rec, Vt, St0),
    {Fvt,St2} = check_record(Line, Name, St1,
			     fun (Dfs) ->
				     record_field(Field, Name, Dfs, Vt, St1)
			     end),
    {vtmerge(Rvt, Fvt),St2};
expr({record,Line,Rec,Name,Upds}, Vt, St0) ->
    {Rvt,St1} = record_expr(Line, Rec, Vt, St0),
    {Usvt,St2} = check_record(Line, Name, St1,
			  fun (Dfs) ->
				  update_fields(Upds, Name, Dfs, Vt, St1)
			  end ),
    {vtmerge(Rvt, Usvt),St2};
expr({block,Line,Es}, Vt, St) ->
    %% Unfold block into a sequence.
    exprs(Es, Vt, St);
expr({'if',Line,Cs}, Vt, St0) ->
    {Cvt,St1} = icr_clauses(Cs, Vt, St0),	%Cvt is new variables.
    All = subtract(vintersection(Cvt), vars(Vt)),
%    All = vintersection(Cvt),
    Some = subtract(vunion(Cvt), vars(Vt)),
%    Some = vunion(Cvt),
    {vtmerge(vtexport(All, 'if', vtunsafe(subtract(Some, All), [])),
	     vtmerge(Cvt)),
     St1};
expr({'case',Line,E,Cs}, Vt, St0) ->
    {Evt,St1} = expr(E, Vt, St0),
    {Cvt,St2} = icr_clauses(Cs, vtupdate(Evt, Vt), St1),
    All = subtract(vintersection(Cvt), vars(Vt)),
%    All = vintersection(Cvt),
    Some = subtract(vunion(Cvt), vars(Vt)),
%    Some = vunion(Cvt),
%    io:format("case ~p~n", [{vt, Vt, evt, Evt, cvt, Cvt}]),
    {vtmerge(vtexport(All, 'case', vtunsafe(subtract(Some, All), [])),
	     vtmerge([Evt|Cvt])),
     St2};
expr({'receive',Line,Cs}, Vt, St0) ->
    {Cvt,St1} = icr_clauses(Cs, Vt, St0),
    All = subtract(vintersection(Cvt), vars(Vt)),
%    All = vintersection(Cvt),
    Some = subtract(vunion(Cvt), vars(Vt)),
%    Some = vunion(Cvt),
    {vtmerge(vtexport(All, 'receive', vtunsafe(subtract(Some, All), [])),
	     vtmerge(Cvt)),
     St1};
expr({'receive',Line,Cs,To,ToEs}, Vt, St0) ->
    %% Are variables from the timeout expression visible in the clauses? NO!
    {Tvt,St1} = expr(To, Vt, St0),
    {Tevt,St2} = exprs(ToEs, Vt, St1),
    {Cvt,St3} = icr_clauses(Cs, Vt, St2),
%    Csvts = [vtnew(Tevt, Vt)|Cvt],		%This is just NEW variables!
    Csvts = [Tevt|Cvt],
    All = subtract(vintersection(Csvts),vars(Vt)),
%    All = vintersection(Csvts),
    Some = subtract(vunion(Csvts),vars(Vt)),
%    Some = vunion(Csvts),
    {vtmerge(vtexport(All, 'receive', vtunsafe(subtract(Some, All), [])),
	     vtmerge([Tvt,Tevt|Cvt])),
     St3};
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
		false ->
		    case {F,A} == St#lint.func of
			true ->
			    {[],St};
			false ->
			    {[],St#lint{called=[{Line,{F,A}} |
						St#lint.called]}}
		    end
	    end
    end;
expr(T={call,Line,{remote,Lr,M,F},As}, Vt, St0) ->
    St1 = format_check(Line, M, F, As, St0),
    St = obsolete_function(Line, M, F, As, St1),
    expr_list([M,F|As], Vt, St);		%They see the same variables
expr({call,Line,{atom,La,record_info},[{atom,Li,Info},{atom,Ln,Name}]},
     Vt, St) ->
    case member(Info, [fields,size]) of
	true -> {[],exist_record(La, Name, St)};
	false -> {[],add_error(Li, record_info, St)}
    end;
expr({call,Line,{atom,La,record_info},[I,N]}, Vt, St) ->
    {[],add_error(Line, record_info, St)};
expr(T={call,Line,{atom,La,F},As}, Vt, St0) ->
    {Asvt,St1} = expr_list(As, Vt, St0),
    A = length(As),
    case erl_internal:bif(F, A) of
	true -> {Asvt,St1};
	false ->
	    {Asvt,case imported(F, A, St1) of
		      {yes,M} ->
			  St2 = format_check(Line, M, F, As, St1),
			  St3 = obsolete_function(Line, M, F, As, St2),
			  St3#lint{imported=add_element({{F,A},M},
							St1#lint.imported)};
		      no ->
			  case {F,A} == St1#lint.func of
			      true ->
				  St1;
			      false ->
				  St1#lint{called=[{Line, {F,A}} |
						   St1#lint.called]}
			  end
		  end}
    end;
expr({call,Line,F,As}, Vt, St0) ->
    St = warn_invalid_call(Line,F,St0),
    expr_list([F|As], Vt, St);			%They see the same variables
expr({'catch',Line,E}, Vt, St0) ->
    %% No new variables added, flag new variables as unsafe.
    {Evt,St1} = expr(E, Vt, St0),
    Uvt = vtunsafe(vars(vtnew(Evt, Vt)), []),
    {vtupdate(vtupdate(Evt, Vt),Uvt),St1};
expr({match,Line,P,E}, Vt, St0) ->
    {Evt,St1} = expr(E, Vt, St0),
    {Pvt,St2} = pattern(P, vtupdate(Evt, Vt), St1),
    %% Must do some work here to get just new stuff.
    Mvt = intersection(vars(Pvt), vunion(Evt, Vt)),	%Matching variables
    {vtmerge(Evt, Pvt),import_vars(Line, Mvt, match, St2)};
%% No comparison or boolean operators yet.
expr({op,Line,Op,A}, Vt, St) ->
    expr(A, Vt, St);
expr({op,Line,Op,L,R}, Vt, St) ->
    expr_list([L,R], Vt, St);			%They see the same variables
%% The following are not allowed to occur anywhere!
expr({remote,Line,M,F}, Vt, St) ->
    {[],add_error(Line, expr, St)};
expr({record_field,Line,Rec,F}, Vt, St) ->
    {[],add_error(Line, expr, St)};
expr({'query',Line,Q}, Vt, St) ->
    {[],add_error(Line, {mnemosyne,"query"}, St)}.

record_expr(Line, Rec, Vt, St0) ->
    St1 = warn_invalid_record(Line, Rec, St0),
    expr(Rec, Vt, St1).

%% valid_record(Record) -> boolean()

valid_record(Rec) ->
    case Rec of
	{atom, _, _} -> false;
	{integer, _, _} -> false;
	{float, _, _} -> false;
	{string, _, _} -> false;
	{cons, _, _, _} -> false;
	{nil, _} -> false;
	{lc, _, _, _} -> false;
	{record_index, _, _, _} -> false;
	{'fun', _, _} -> false;
	_ ->
	    true
    end.

%% warn_invalid_record(Line, Record, State0) -> State
%% Adds warning if the record is invalid.

warn_invalid_record(Line, R, St) ->
    case valid_record(R) of
	true ->
	    St;
	false ->
	    add_warning(Line, invalid_record, St)
    end.

%% valid_call(Call) -> boolean()

valid_call(Call) ->
    case Call of
	{integer, _, _} -> false;
	{float, _, _} -> false;
	{string, _, _} -> false;
	{cons, _, _, _} -> false;
	{nil, _} -> false;
	{lc, _, _, _} -> false;
	{record_index, _, _, _} -> false;
	{tuple, _, Exprs} when length(Exprs) /= 2 -> false;
	_ ->
	    true
    end.

%% warn_invalid_call(Line, Call, State0) -> State
%% Adds warning if the call is invalid.

warn_invalid_call(Line, F, St) ->
    case valid_call(F) of
	true ->
	    St;
	false ->
	    add_warning(Line, invalid_call, St)
    end.

%% expr_list(Expressions, Variables, State) ->
%%      {UsedVarTable,State}

expr_list(Es, Vt, St) ->
    foldl(fun (E, {Esvt,St0}) ->
		  {Evt,St1} = expr(E, Vt, St0),
		  {vtmerge(Evt, Esvt),St1}
	  end, {[],St}, Es).

%% record_def(Line, RecordName, [RecField], State) -> State.
%%  Add a record definition if it does not already exist. Normalise
%%  so that all fields have explicit initial value.

record_def(Line, Name, Fs0, St0) ->
    case dict:is_key(Name, St0#lint.records) of
	true -> add_error(Line, {redefine_record,Name}, St0);
	false ->
	    {Fs1,St1} = def_fields(normalise_fields(Fs0), Name, St0),
	    St1#lint{records=dict:store(Name, Fs1, St1#lint.records)}
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
    case dict:is_key(Name, St#lint.records) of
	true -> St;
	false -> add_error(Line, {undefined_record,Name}, St)
    end.

%% check_record(Line, RecordName, State, CheckFun) ->
%%	{UpdVarTable,State}.
%%  The generic record checking function, first checks that the record
%%  exists then calls the specific check function.  N.B. the check
%%  function can safely assume that the record exists.
%%
%%  The check function is called:
%%	CheckFun(RecordDefFields)
%%  and must return
%%	{UpdatedVarTable,State}

check_record(Line, Name, St, CheckFun) ->
    case dict:find(Name, St#lint.records) of
	{ok,Fields} -> CheckFun(Fields);
	error -> {[],add_error(Line, {undefined_record,Name}, St)}
    end.

%%% Record check functions.

%% check_fields([ChkField], RecordName, [RecDefField], VarTable, State, CheckFun) ->
%%	{UpdVarTable,State}.

check_fields(Fs, Name, Fields, Vt, St0, CheckFun) ->
    {Seen,Uvt,St1} =
	foldl(fun (Field, {Sfsa,Vta,Sta}) ->
		      {Sfsb,{Vtb,Stb}} = check_field(Field, Name, Fields,
						     Vt, Sta, Sfsa, CheckFun),
		      {Sfsb,vtmerge(Vta, Vtb),Stb}
	      end, {[],[],St0}, Fs),
    {Uvt,St1}.

check_field({record_field,Lf,{atom,La,F},Val}, Name, Fields,
	    Vt, St, Sfs, CheckFun) ->
    case member(F, Sfs) of
	true -> {Sfs,{Vt,add_error(Lf, {redefine_field,Name,F}, St)}};
	false ->
	    {[F|Sfs],
	     case find_field(F, Fields) of
		 {ok,I} -> CheckFun(Val, Vt, St);
		 error -> {[],add_error(La, {undefined_field,Name,F}, St)}
	     end}
    end.

%% pattern_field(Field, RecordName, [RecDefField], VarTable, State) ->
%%	{UpdVarTable,State}.
%%  Test if record RecordName has field Field. Set State.

pattern_field({atom,La,F}, Name, Fields, Vt, St) ->
    case find_field(F, Fields) of
	{ok,I} -> {[],St};
	error -> {[],add_error(La, {undefined_field,Name,F}, St)}
    end.

%% pattern_fields([PatField], RecordName, [RecDefField], VarTable, State) ->
%%	{UpdVarTable,State}.

pattern_fields(Fs, Name, Fields, Vt, St) ->
    check_fields(Fs, Name, Fields, Vt, St, fun pattern/3).

%% record_field(Field, RecordName, [RecDefField], VarTable, State) ->
%%	{UpdVarTable,State}.
%%  Test if record RecordName has field Field. Set State.

record_field({atom,La,F}, Name, Fields, Vt, St) ->
    case find_field(F, Fields) of
	{ok,I} -> {[],St};
	error -> {[],add_error(La, {undefined_field,Name,F}, St)}
    end.

%% init_fields([InitField], InitLine, RecordName, [DefField], VarTable, State) ->
%%	{UpdVarTable,State}.
%% ginit_fields([InitField], InitLine, RecordName, [DefField], VarTable, State) ->
%%	{UpdVarTable,State}.
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
	{record_field,Lf,{atom,La,F},Di} <- Dfs,
	not exist_field(F, Ifs) ] ++ Ifs.

%% update_fields(UpdFields, RecordName, RecDefFields, VarTable, State) ->
%%	{UpdVarTable,State}

update_fields(Ufs, Name, Dfs, Vt, St) ->
    check_fields(Ufs, Name, Dfs, Vt, St, fun expr/3).

%% exist_field(FieldName, [Field]) -> bool().
%%  Find a record field in a field list.

exist_field(F, [{record_field,Lf,{atom,La,F},Val}|Fs]) -> true;
exist_field(F, [_|Fs]) -> exist_field(F, Fs);
exist_field(F, []) -> false.

%% find_field(FieldName, [Field]) -> {ok,Val} | error.
%%  Find a record field in a field list.

find_field(F, [{record_field,Lf,{atom,La,F},Val}|Fs]) -> {ok,Val};
find_field(F, [_|Fs]) -> find_field(F, Fs);
find_field(F, []) -> error.

%% icr_clauses(Clauses, ImportVarTable, State) ->
%%      {NewVts,State}.

icr_clauses([{clause,Line,H,G,B}|Cs], Vt0, St0) ->
    {Hvt,St1} = head(H, Vt0, St0),
    Vt1 = vtupdate(Hvt, Vt0),
    {Gvt,St2} = guard(G, Vt1, St1),
    Vt2 = vtupdate(Gvt, Vt1),
    {Evt,St3} = exprs(B, Vt2, St2),
    Vt3 = vtupdate(Evt, Vt2),
    {Cvts,St4} = icr_clauses(Cs, Vt0, St3),
%    {[vtnew(Vt3, Vt0)|Cvts],St4};
    {[Vt3|Cvts],St4};
icr_clauses([], Vt, St) ->
    {[],St}.

%% lc_quals(Qualifiers, ImportVarTable, State) ->
%%      {VarTable,State}
%%  Test list comprehension qualifiers, returns all variables. Allow
%%  filters to be both guard tests and general expressions, but the errors
%%  will be for expressions. Return the complete updated vartable, but
%%  this should not cause any problems.

lc_quals([{generate,Line,P,E}|Qs], Vt0, St0) ->
    {Evt,St1} = expr(E, Vt0, St0),
    Vt1 = vtupdate(Evt, Vt0),
    {Pvt,St2} = pattern(P, Vt1, St1),
    St3 = shadow_vars(Line, vars(vtold(Pvt, Vt1)), generate, St2),
    Vt2 = vtupdate(Pvt, Vt1),
    lc_quals(Qs, Vt2, St3);
lc_quals([F|Qs], Vt, St0) ->
    {Fvt,St1} = case is_guard_test(F) of
		    true -> guard_test(F, Vt, St0);
		    false -> expr(F, Vt, St0)
		end,
    lc_quals(Qs, vtupdate(Fvt, Vt), St1);
lc_quals([], Vt, St) -> {Vt,St}. 

%% fun_clauses(Clauses, ImportVarTable, State) ->
%%	{UsedVars, State}.
%%  Fun's cannot export any variables.

fun_clauses(Cs, Vt, St) ->
    foldl(fun ({clause,Line,H,G,B}, {Bvt, St0}) ->
		  {Hvt,St1} = head(H, Vt, St0),
		  St2 = shadow_vars(Line, vars(vtold(Hvt, Vt)), 'fun', St1),
		  Vt1 = vtupdate(Hvt, Vt),
		  {Gvt,St3} = guard(G, Vt1, St2),
		  Vt2 = vtupdate(Gvt, Vt1),
		  {Evt,St4} = exprs(B, Vt2, St3),
		  Cvt = vtupdate(Evt, Vt2),
		  St5 = report_unused(vtnew(Cvt, Vt), St4),
		  {vtmerge(vtsubtract(vtold(Cvt, Vt),Hvt), Bvt), St5}
	  end, {[], St}, Cs).

%% For storing the variable table we use our own version of the module
%% dict. This is/was the same as the original but we must be sure of the
%% format (sorted list of pairs) so we can do ordset operations on them.
%% We know an empty set is [].

%% pat_var(Variable, LineNo, VarTable, State) ->
%%	{UpdVarTable,State'}
%%  A pattern variable has been found. Handle errors and warnings. Return
%%  all variables as bound so errors and warnings are only reported once.

pat_var(V, Line, Vt, St) ->
    case find(V, Vt) of
	{ok,{bound, Used}} -> {[{V,{bound, used}}],St};
	{ok,{unsafe, Used}} ->
	    {[{V,{bound, used}}],add_error(Line, {unsafe_var,V}, St)};
	{ok,{{export,From}, Used}} ->
	    {[{V, {bound, used}}],add_warning(Line, {exported_var,V,From}, St)};
	error -> {[{V,{bound, {unused, Line}}}],St}
    end.

%% expr_var(Variable, LineNo, VarTable, State) ->
%%	{UpdVarTable,State}
%%  Check if a variable is defined, or if there is an error or warning
%%  connected to its usage. Return all variables as bound so errors and
%%  warnings are only reported once.

expr_var(V, Line, Vt, St) ->
    case find(V, Vt) of
	{ok,{bound, Used}} -> {[{V,{bound, used}}],St};
	{ok,{unsafe, Used}} ->
	    {[{V,{bound, used}}],add_error(Line, {unsafe_var,V}, St)};
	{ok,{{export,From}, Used}} ->
	    {[{V,{bound, used}}],add_warning(Line, {exported_var,V,From}, St)};
	error ->
	    {[{V,{bound, used}}],add_error(Line, {unbound_var,V}, St)}
    end.

shadow_vars(Line, Vs, In, St0) ->
    foldl(fun (V, St) -> add_warning(Line, {shadowed_var,V,In}, St) end,
	  St0, Vs).

import_vars(Line, Vs, In, St0) ->
    foldl(fun (V, St) -> add_warning(Line, {imported_var,V,In}, St) end,
	  St0, Vs).

%% vtupdate(UpdVarTable, VarTable) -> VarTable.
%%  Add the variables in the updated vartable to VarTable. The variables
%%  will be updated with their property in UpdVarTable.

vtupdate(Uvt, Vt0) ->
    foldl(fun ({V,How}, Vt) -> store1(V, How, Vt) end, Vt0, Uvt).

%% vtexport([Variable], From, VarTable) -> VarTable.
%% vtunsafe([Variable], VarTable) -> VarTable.
%%  Add the variables to VarTable either as exported from From or as unsafe.

vtexport(Uvt, From, Vt0) ->
    foldl(fun (V, Vt) -> store1(V, {{export,[From]}, {unused, 0}}, Vt) end, Vt0, Uvt).

vtunsafe(Uvt, Vt0) ->
    foldl(fun (V, Vt) -> store1(V, {unsafe, {unused, 0}}, Vt) end, Vt0, Uvt).

store1(V, Val, Tab) ->
    {A, Used} = Val,
    case find(V, Tab) of
	{ok, {_, Used0}} ->
	    store(V, {A, merge_used(Used0, Used)}, Tab);
	error ->
	    store(V, Val, Tab)
    end.

%% vtmerge(VarTable, VarTable) -> VarTable.
%%  Merge two variables tables generating a new vartable.  Give prioriy to
%%  errors then warnings.

merge_used(Used1, Used2) ->
    if
	Used1 == used -> used;
	Used2 == used -> used;
	Used1 < Used2 -> Used2;
	true -> Used1
    end.

vtmerge([{V1,Val1}|Vs1], [{V2,Val2}|Vs2]) when V1 < V2 ->
    [{V1,Val1}|vtmerge(Vs1, [{V2,Val2}|Vs2])];
vtmerge([{V1,Val1_0}|Vs1], [{V2,Val2_0}|Vs2]) when V1 == V2 ->
    {Val1, Used1} = Val1_0,
    {Val2, Used2} = Val2_0,
    Used = merge_used(Used1, Used2),
    V = if
	    Val1 == unsafe -> {V1,unsafe};	%Take the error case
	    Val2 == unsafe -> {V2,unsafe};
	    Val1 == bound -> {V2,Val2};		%Take the warning
	    Val2 == bound -> {V1,Val1};
	    true ->				%Both are exports
		{V1,{export,element(2, Val1) ++ element(2, Val2)}}
	end,
    {A,B}=V,
    Vtot={A,{B,Used}},
    [Vtot|vtmerge(Vs1, Vs2)];
vtmerge([{V1,Val1}|Vs1], [{V2,Val2}|Vs2]) when V1 > V2 ->
    [{V2,Val2}|vtmerge([{V1,Val1}|Vs1], Vs2)];
vtmerge([], Vs2) -> Vs2;
vtmerge(Vs1, []) -> Vs1.

vtmerge_pat([{V1,Val1}|Vs1], [{V2,Val2}|Vs2]) when V1 < V2 ->
    [{V1,Val1}|vtmerge_pat(Vs1, [{V2,Val2}|Vs2])];
vtmerge_pat([{V1,Val1_0}|Vs1], [{V2,Val2_0}|Vs2]) when V1 == V2 ->
    {Val1, Used1} = Val1_0,
    {Val2, Used2} = Val2_0,
    Used = used,
    V = if
	    Val1 == unsafe -> {V1,unsafe};	%Take the error case
	    Val2 == unsafe -> {V2,unsafe};
	    Val1 == bound -> {V2,Val2};		%Take the warning
	    Val2 == bound -> {V1,Val1};
	    true ->				%Both are exports
		{V1,{export,element(2, Val1) ++ element(2, Val2)}}
	end,
    {A,B}=V,
    Vtot={A,{B,Used}},
    [Vtot|vtmerge_pat(Vs1, Vs2)];
vtmerge_pat([{V1,Val1}|Vs1], [{V2,Val2}|Vs2]) when V1 > V2 ->
    [{V2,Val2}|vtmerge_pat([{V1,Val1}|Vs1], Vs2)];
vtmerge_pat([], Vs2) -> Vs2;
vtmerge_pat(Vs1, []) -> Vs1.

vtmerge(Vts) -> foldl(fun (Vt, Mvts) -> vtmerge(Vt, Mvts) end, [], Vts).

%% vtnew(NewVarTable, OldVarTable) -> NewVarTable.
%%  Return all the truly new variables in NewVarTable.

vtnew(New, Old) ->
    filter(fun ({V,How}) -> not is_key(V, Old) end, New).

%% vtsubtract(VarTable1, VarTable2) -> NewVarTable.
%%  Return all the variables in VarTable1 which don't occur in VarTable2.
%%  Same thing as vtnew, but a more intuitive name for some uses.
vtsubtract(New, Old) ->
    vtnew(New, Old).

%% vtold(NewVarTable, OldVarTable) -> OldVarTable.
%%  Return all the truly old variables in NewVarTable.

vtold(New, Old) ->
    filter(fun ({V,How}) -> is_key(V, Old) end, New).

vars(Vt) -> [ V || {V,How} <- Vt ].

vunion(Vs1, Vs2) -> union(vars(Vs1), vars(Vs2)).

vunion(Vss) -> foldl(fun (Vs, Uvs) -> union(vars(Vs), Uvs) end, [], Vss).

-ifdef(NOTUSED).
vintersection(Vs1, Vs2) -> intersection(vars(Vs1), vars(Vs2)).
-endif.

vintersection([Vs]) -> vars(Vs);		%Boundary conditions!!!
vintersection([Vs|Vss]) -> intersection(vars(Vs), vintersection(Vss));
vintersection([]) -> [].

%% This is our own version of the module dict which is/was the same as
%% the original but we must be sure of the format (sorted list of pairs)
%% so we can do ordset operations on them. We know an empty set is [].

%% is_key(Key, Dictionary) -> bool().

is_key(Key, [{K,Val}|D]) when Key > K -> is_key(Key, D);
is_key(Key, [{K,Val}|D]) when Key == K -> true;
is_key(Key, [{K,Val}|D]) when Key < K -> false;
is_key(Key, []) -> false.

%% find(Key, Dictionary) -> {ok,Value} | error

find(Key, [{K,Value}|D]) when Key > K -> find(Key, D);
find(Key, [{K,Value}|_]) when Key == K -> {ok,Value};
find(Key, [{K,Value}|_]) when Key < K -> error;
find(Key, []) -> error.

%% store(Key, Value, Dictionary) -> Dictionary.

store(Key, New, [{K,Old}|Dict]) when Key > K ->
    [{K,Old}|store(Key, New, Dict)];
store(Key, New, [{K,Old}|Dict]) when Key == K ->
    [{Key,New}|Dict];
store(Key, New, [{K,Old}|Dict]) when Key < K ->
    [{Key,New},{K,Old}|Dict];
store(Key, New, []) -> [{Key,New}].

%% copy_expr(Expr, Line) -> Expr.
%%  Make a copy of Expr converting all line numbers to Line.

copy_expr({clauses,Cs}, Line) -> {clauses,copy_expr(Cs, Line)};
copy_expr({function,F,A}, Line) -> {function,F,A};
copy_expr({Tag,L}, Line) -> {Tag,Line};
copy_expr({Tag,L,E1}, Line) ->
    {Tag,Line,copy_expr(E1, Line)};
copy_expr({Tag,L,E1,E2}, Line) ->
    {Tag,Line,copy_expr(E1, Line),copy_expr(E2, Line)};
copy_expr({Tag,L,E1,E2,E3}, Line) ->
    {Tag,Line,
     copy_expr(E1, Line),
     copy_expr(E2, Line),
     copy_expr(E3, Line)};
copy_expr({Tag,L,E1,E2,E3,E4}, Line) ->
    {Tag,Line,
     copy_expr(E1, Line),
     copy_expr(E2, Line),
     copy_expr(E3, Line),
     copy_expr(E4, Line)};
copy_expr([H|T], Line) ->
    [copy_expr(H, Line)|copy_expr(T, Line)];
copy_expr([], Line) -> [];
copy_expr(E, Line) when constant(E) -> E.

%% Add warning for calls to obsolete functions.
    
obsolete_function(Line, {atom, _, Mod}, {atom, _, Func}, As, St) ->
    obsolete_function(Line, Mod, Func, As, St);
obsolete_function(Line, Mod, Func, As, St) ->
    Arity = length(As),
    case otp_internal:obsolete(Mod, Func, Arity) of
	false ->
	    St;
	{true, Info} ->
	    add_warning(Line, {obsolete, {Mod, Func, Arity}, Info}, St)
    end.

%%% Format checking

format_check(Line, {atom, _, Mod}, {atom, _, Func}, As, St) ->
    format_check(Line, Mod, Func, As, St);
format_check(Line, M, F, As, St0) ->
    case is_format_like(M, F, As) of
	true ->
	    case lists:keysearch(warn_format, 1, St0#lint.compile) of
		{value, {warn_format, Verbosity}} ->
		    case check_format(As) of
			{Level, A, B} when Level =< Verbosity ->
			    add_warning(Line, {format, {A, B}}, St0);
			_ ->
			    St0
		    end;
		false ->
		    St0
	    end;
	false ->
	    St0
    end.

listify_args({nil, _}) ->
    [];
listify_args({cons, _, Car, Cdr}) ->
    [Car | listify_args(Cdr)];
listify_args(X) ->
    X.

proper_list_p([_|Rest]) ->
    proper_list_p(Rest);
proper_list_p([]) ->
    true;
proper_list_p(_) ->
    false.

module_with_format(io) ->
    true;
module_with_format(io_lib) ->
    true;
module_with_format(_) ->
    false.

format_like_function(format) ->
    true;
format_like_function(fwrite) ->
    true;
format_like_function(_) ->
    false.

is_format_like(M, F, A) ->
    case module_with_format(M) of
	true ->
	    format_like_function(F);
	false ->
	    false
    end.		    

check_format([Fmt]) ->
    check_format1(Fmt, []);
check_format([Fmt, Rest0]) ->
    Rest = listify_args(Rest0),
    case proper_list_p(Rest) of
	false ->
	    {2, "arguments perhaps not a list in format-like call", []};
	true ->
	    check_format1(Fmt, Rest)
    end;
check_format([Var, Fmt, Rest]) ->
    check_format([Fmt, Rest]);
check_format([Fmt | Rest]) ->
    {2, "format-like call with wrong number of arguments", []}.

check_format1(Fmt, Args) ->
    case Fmt of
	{string, _, String} ->
	    check_format11(String, Args);
	{atom, _, Atom} ->
	    check_format11(atom_to_list(Atom), Args);
	Other ->
	    {2, "format string not a textual constant", []}
    end.

check_format11(Fmt, Args) ->
    Dirs = extract_directives(Fmt),
    if
	element(1, Dirs) == error ->
	    {error, E} = Dirs,
	    {1, "invalid format string (~s)", [E]};
	length(Dirs) == length(Args) ->
	    ok;
	list(Dirs) ->
	    {1, "wrong number of arguments in format-like call", []}
    end.

extract_directives(String) ->
    Tilde = string:chr(String, $~),
    case Tilde of
	0 ->
	    [];
	Pos ->
	    String1 = string:substr(String, Pos),
	    case analyse_directive(String1) of
		{error, E} ->
		    {error, E};
		{Dirs, String2} ->
		    case extract_directives(String2) of
			{error, E} ->
			    {error, E};
			Dirs2 ->
			    Dirs ++ Dirs2
		    end
	    end
    end.

%%% We do one thing wrong here: we don't handle padding characters.
%%% To do that, we need to inspect what comes after the second period.

analyse_directive("~") ->
    {error, "truncated"};
analyse_directive([$~ | Rest]) ->
    analyse_directive1(Rest).

analyse_directive1([C | Rest]) when C >= $0, C =< $9 ->
    analyse_directive1(Rest);
analyse_directive1([$- | Rest]) ->
    analyse_directive1(Rest);
analyse_directive1([$. | Rest]) ->
    analyse_directive1(Rest);
analyse_directive1([$* | Rest]) ->
    {D, Rest1} = analyse_directive1(Rest),
    {[int | D], Rest1};
analyse_directive1([C | Rest]) ->
    case format_dir_type(C) of
	{error, E} ->
	    {error, E};
	Other ->
	    {Other, Rest}
    end.

format_dir_type($~) ->
    [];
format_dir_type($c) ->
    [int];
format_dir_type($f) ->
    [float];
format_dir_type($e) ->
    [float];
format_dir_type($g) ->
    [float];
format_dir_type($s) ->
    [string];
format_dir_type($w) ->
    [term];
format_dir_type($p) ->
    [term];
format_dir_type($W) ->
    [term, int];
format_dir_type($P) ->
    [term, int];
format_dir_type($n) ->
    [];
format_dir_type($i) ->
    [term];
format_dir_type(C) ->
    {error, "invalid directive " ++ [C]}.
