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
%% Purpose : Compiler for Erlang BEAM

-module(v1_compile).

-export([module/2]).
-export([format_error/1]).

-export([get_var_type/2, new_reg/1, symbol_table/1,
	 new_lb/0, new_next_pattern_lb/0]).

-export([print_table/1]).
-export([l_func_table/2]).

-export([explain_env/1]).

-import(lists, [member/2, append/2, append/1, flatten/1, concat/1]).
-import(lists, [reverse/1]).

-import(v1_optimize, [optimize_comp/6]).
-import(v1_optimize, [optimize_arith/6]).
-import(erl_internal, [guard_bif/2]).

module({Mod,Exports,Attr,Forms}, Opts) ->
    put(c_l_func, []),				% BH symbol table for local func
    put(c_next_label, 1),			% Next label.
    put(c_current_label, 1),			% Current label.
    case member(verbose, Opts) of
	true ->
	    io:format("Erlang BEAM Compiler ~s/~w~n",
		      [erlang:info(version), beam_opcodes:format_number()]);
	_ ->
	    ok
    end,
    do_compile(Mod, Exports, Attr, Forms, Opts, [], []).

do_compile(Mod, Exports, Attr, [{function, Line, Name, Arity, Clauses}|T], 
	   Opts, Errs, CodeBuff) ->
    Form = {function, Line, Name, Arity, Clauses},
%%    io:format("compiling: ~w:~w/~w\n", [Mod, Name, Arity]),
    Type = case lists:member({Name,Arity}, Exports) of
	       true  -> exported;
	       false -> local
	   end,
    case function(Mod, Form, Opts) of
	{code, C} ->
	    C1 = {code, Mod, Name, Arity, Type, C},
	    do_compile(Mod, Exports, Attr, T, Opts, Errs, [C1|CodeBuff]);
	{error, What} ->
	    do_compile(Mod, Exports, Attr, T, Opts, What++Errs, [])
    end;
do_compile(Mod, Exports, Attr, [{asm, Name, Arity, Code} | T],
	   Opts, Errs, CodeBuff) ->
    Type = case lists:member({Name,Arity}, Exports) of
	       true  -> exported;
	       false -> local
	   end,
    Entry = l_func_table(Name, Arity),
    C1 = {asm, Mod, Name, Arity, Type, Entry, Code},
    do_compile(Mod, Exports, Attr, T, Opts, Errs, [C1 | CodeBuff]);
do_compile(Mod, Exports, Attr, [H|T], Opts, Errs, CodeBuff) ->
    do_compile(Mod, Exports, Attr, T, Opts, 
	       [{none,?MODULE,{unknown_form,H}}|Errs], CodeBuff);
do_compile(Mod, Exports, Attr, [], Opts, [], CodeBuff) ->
    {ok, {Mod, Exports, Attr, reverse(CodeBuff), get(c_next_label)}};
do_compile(Mod, Exports, Attr, [], Opts, Errs, CodeBuff) ->
    {error, Errs}.

%______________________________________________________________________
%% function(Mod, Func, Options) -  compile a function
%%   Func is an abstract form returned by the parser
%%   Options is a list of options from the user
%%   -> {code,Code}
%%   -> {error, Type, What}

function(Mod, Func, Options) ->
    case 
	catch					% can be commented out when debugging!
	function1(Mod, Func, Options) of
	{code,X}   -> {code,X};
	{error,X} -> {error,[X]};
	{'EXIT',X} -> {error,[{none,{?MODULE,{'EXIT',X}}}]}
    end.

function1(Mod, Func, Options) ->
    {Code,E1} = compile_clauses(Mod,Func,new_env(Func, Options)), 
    {code,Code}.

%______________________________________________________________________

new_env(Func,Options) ->
    MaxReg = 0,
    LocalVar = 0,
    AddAllocate = 0, % number of fun calls and bif_gc
    ClauseFail = false,
    SymbolTable = [],
    Where = [],
    VarList = [], % list of variables to be saved
    ExpVarList = [], % list of variables to be exported from options
    FuncName = {0,0}, % index in atom table with function name, arity
    MaxArg = 1024,
    RetVal = false, % flag stating if ret value is required
    MaxRegNumber = 1024,
    PatternFail = [],
    {env,dummy,VarList,Options,Func,LocalVar,AddAllocate,
     MaxReg,ClauseFail,SymbolTable,Where,ExpVarList,FuncName,
     MaxArg,RetVal,MaxRegNumber,PatternFail}.

explain_env(E) ->
    {env, [{var_to_be_saved, var_to_be_saved(E)},
	   {local_var, local_var(E)},
	   {allocate, allocate(E)},
	   {next_reg, element(8, E)},
	   {clause_fail, clause_fail(E)},
	   {where, element(11, E)},
	   {var_to_be_exported, var_to_be_exported(E)},
	   {ret_val, ret_val(E)},
	   {pattern_fail, element(17, E)}]}.
     

%______________________________________________________________________
%% The Environment -- usually abbreviated to E !

var_to_be_saved(E) ->	        element(3,E).
set_var_to_be_saved(E,V) ->	setelement(3,E,V).

debug_flag(E) ->	member(debug, element(4,E)).
get_function(E) ->	element(5,E).
get_arity(E) ->         element(4,get_function(E)).

new_local_var(E) ->    	Var = element(6,E),
	       		{Var,setelement(6,E,Var + 1)}.
set_local_var(E,N) ->  	setelement(6,E,N).
local_var(E) ->         element(6,E).

add_allocate(E) ->     	setelement(7,E,allocate(E)+1).
clear_allocate(E) ->   	setelement(7,E,0).
allocate(E) ->	        element(7,E).
set_allocate(E,N) ->    setelement(7,E,N).

new_reg(E) ->    	Reg = element(8,E),
			MaxReg = max_reg(E),
			if 
			    Reg > MaxReg ->
				Error = append(["register_number_",
						integer_to_list(Reg),
						"_larger_than_allowed_", 
						integer_to_list(MaxReg)]),
				throw({error, {none,?MODULE,Error}});
			    true ->
				case check_value(symbol_table(E), {x, Reg}) of
				    {_} -> 
					new_reg(setelement(8,E,Reg + 1));
				    {_,_} -> 
					new_reg(setelement(8,E,Reg + 1));
				    not_found ->
					{Reg,setelement(8,E,Reg + 1)}
				end
			end.

set_reg(E,Value)  ->    setelement(8,E,Value).

% cond_set_reg/2, it is not allowed to reuse registers
% while compiling options (due to common instructions optimization)
% and clauses heads
cond_set_reg(E,Value) ->     case where_info(E) of
				 {head_case,_} -> E;
				 head -> E;
				 _ -> setelement(8,E,Value)
			     end.

% used to detect failing options in clauses, cases, if, receive
add_fail(E) ->   case where_info(E) of
			    head -> set_clause_fail(E);
			    {head_case,_} -> set_pattern_fail(E);
			    _ -> E
			end.

clause_fail(E) ->       element(9,E).
clear_clause_fail(E) -> setelement(9,E,false).
set_clause_fail(E) -> setelement(9,E,true).

new_symbol_table(E) ->  setelement(10,E,[]).
symbol_table(E) ->      element(10,E).
save_symbol_table(T,E) -> setelement(10,E,T). 

where(E) ->	        Where = hd(element(11,E)),
			case Where of
			    head -> {Where,next_lb()};
			    _ -> Where
			end.

% set_where/reset_where sets/resets pattern_fail as well
set_where(E,Value) ->   E0 = setelement(11,E,[Value | element(11,E)]),
			clear_pattern_fail(E0).
reset_where(E) ->       E0 = setelement(11,E,tl(element(11,E))),
			reset_pattern_fail(E0).
where_info(E) ->        hd(element(11,E)).

var_to_be_exported(E) ->        element(12,E).
set_var_to_be_exported(E,V) ->	set_local_var(setelement(12,E,V),length(V)).

func_name(E) ->        element(13,E).
set_func_name(F, E) ->  setelement(13, E, {F, get_arity(E)}).

max_arg(E) ->           element(14,E).

% indicates if options value is required
ret_val(E) ->           element(15,E).
set_ret_val(F,E) ->     setelement(15,E,F).

max_reg(E) ->           element(16,E).

pattern_fail(E) ->       hd(element(17,E)).
clear_pattern_fail(E) -> setelement(17,E,[false | element(17,E)]).
set_pattern_fail(E) ->   setelement(17,E,[true | tl(element(17,E))]).
reset_pattern_fail(E) -> setelement(17,E,tl(element(17,E))).

new_next_lb() ->
    Label = get(c_next_label),
    put(c_next_label, Label+1),
    put(c_current_label, Label),
    Label.

next_lb() ->
    get(c_current_label).

new_lb() ->
    Label = get(c_next_label),
    put(c_next_label, Label+1),
    Label.

new_next_pattern_lb() ->
    new_lb().

% insert_symbol_table(Var,Value0,E -> ({ok,E'} | {conflict,Value1})
% Value0 = ({x/y,N} | {{x/y,N},Type} | nil | {atom,N} | {i,N}) | msg | {msg,Type}
% insert element Var:Value0 into symbol table
% add type for {a,N} and {i,N}
insert_symbol_table(Var,Value0,E) ->
    Table = symbol_table(E),
    case table_lookup(Var, Table) of
	not_found -> 
	    case Value0 of
		{i,N} ->
		    {ok,save_symbol_table(
			      table_insert(Var,{Value0,integer},Table),E)}; 
		{atom,_} -> {ok,save_symbol_table(
			      table_insert(Var,{Value0,atom},Table),E)}; 
		_ -> {ok,save_symbol_table(table_insert(Var,Value0,Table),E)}
	    end;
	{found, Value0} -> {ok,E};
	{found, {Value0,_}} -> {ok,E};
	{found, {{R,N},Type}} -> {conflict,{R,N},Type};
	{found, Value1} -> {conflict,Value1,no}
    end.

% lookup_symbol_table(Var,E) -> ({found,Value} | not_found)
% get value of Var from symbol table
lookup_symbol_table(Var,E) ->
    Table = symbol_table(E),
    table_lookup(Var, Table).

% check_value(Table, Value) -> ({Key} | {Type,Key} | not_found)
% check if there is Value in symbol table Table
% return Type if any
check_value([{Key, Value}|_], Value) ->
    {Key};
check_value([{Key, {Value, Type}}|_], Value) ->
    {Type, Key};
check_value([_|T], Value) ->
    check_value(T, Value);
check_value([], _) ->
    not_found.

% can_be_reused(R,E) -> (true | false)
% check if R can be reused
% R cannot be reused if it is argument register, variable value, local variable
% {x,0} cannot be reused while matching in case after calling a function
% due to common instructions optimizations (both in clauses and options)
%   we cannot reuse registers in heads
can_be_reused(X,E) ->  
    case where_info(E) of
	{head_case,_} -> false;
	head -> false;
	_ -> can_be_reused0(X,E)
    end.

can_be_reused0({x,N0},E) ->    	
    N1 = get_arity(E),
    if 
	N0 < N1 -> false;        %argument register
	N0 >= N1 ->
	    case check_value(symbol_table(E), {x, N0}) of
		{_} -> false;      %local variable
		{_,_} -> false;    %local variable
		not_found ->
	             case where(E) of
			 {head_case,_} when N0 == 0 -> false;
			 _ -> true
		     end
	    end
    end;
can_be_reused0({y,_},_) -> false;
can_be_reused0(_,_) -> true.

% type_var(VarName,Type,E) -> (E' | error)
% insert variable type into symbol table
type_var(VarName,no,E) -> E;
type_var(VarName,Type,E) ->
    Table = symbol_table(E),
    case table_lookup(VarName, Table) of
	{found, {{_,_},Type}} -> E;
	{found, {{_,_},_}} -> E; % error in program logic, will fail earlier
	{found, Value} -> 
	    save_symbol_table(table_insert(VarName,{Value,Type},Table),E);
	not_found -> throw({error,{none,?MODULE,
				   trying_to_type_non_existent_variable}})
    end.

% get_var_type(Reg,E) -> (Type | no)
% Reg = ({x,N} | {y,N})
% get variable type from symbol table
get_var_type(Reg,E) ->
    case check_value(symbol_table(E), Reg) of
	{Type,_} -> Type;
	{_} -> no;
	not_found -> throw({error,{none,?MODULE,
				   {undefined_variable,Reg,get_function(E)}}})
    end.


%______________________________________________________________________
% compile_clauses(Module,Func,Env) -> {Code, Env'}

compile_clauses(Module,{function,Line,Name,Arity,Clauses},E0) ->
    MaxArg = max_arg(E0),
    if 
	Arity > MaxArg ->
	    Error = append("arity_larger_than_allowed", 
			   integer_to_list(MaxArg)),
	    throw({error,{Line,?MODULE,
			  {list_to_atom(Error),Module,Name,Arity}}});
	true ->
	    ok
    end,
    Lb = l_func_table(Name, Arity),
    %%io:format("~nClause0: ~w~n",[Clauses]),
    compile_clause_list(Clauses, set_func_name(Name,E0), Name, Arity, Lb).

% some one-clause functions can have tests and need the structure
%    Clause;
%    ...
%    ClauseEnd(Lb);
%    ERROR_ACTION(FUNCTION_CLAUSE);
compile_clause_list([X],E0,Name,Arity, FuncInfo) ->
    Lb = new_next_lb(),
    {Code,E1} = compile_clause(X,E0),
    case clause_fail(E1) of
	true ->
	    {[{label, Name, Arity, FuncInfo, E1},
	      [flatten([{clause_header,Arity}, Code,{clause_end,Lb}])],
	      {error_func_clause,
	       {Name, get_arity(E0)}}],
	     E1};
	false ->
	    {[{label, Name, Arity, FuncInfo, E1}, [flatten(Code)]],E1}
    end;
compile_clause_list([H|T],E0,Name,Arity, FuncInfo) ->
    Lb = new_next_lb(),
    {Code1,E1} = compile_clause(H,E0),
    {Code2,E2} = compile_next_clause(T,E1),
    case clause_fail(E2) of
	true ->
	    {[{label,Name,Arity,FuncInfo,E2},
	      [flatten([{clause_header,Arity},Code1,
			{clause_end,Lb}])|Code2],
	      {error_func_clause,
	       {Name, get_arity(E0)}}],
	     E2};
	false ->
	    {[{label, Name, Arity, FuncInfo, E2},
	      [flatten([{clause_header,Arity},Code1,
			{clause_end,Lb}])|Code2]], E2}
    end.

% if the last clause cannot fail do not generate error_func_clause
% and generate {clause_end,0} which is omitted in threaded code anyway
compile_next_clause([H],E0) ->
        Lb = new_next_lb(),
        E1 = clear_clause_fail(E0),
        {Code0,E2} = compile_clause(H,E1),
        case clause_fail(E2) of
	    true ->
		{[flatten([{clause_header,get_arity(E0)},Code0,
			   {clause_end,Lb}])],E2};
	    false ->
		{[flatten([{clause_header,get_arity(E0)},Code0,
			   {clause_end,0}])],E2}
	end;
compile_next_clause([H|T],E0) ->
        Lb = new_next_lb(),
        {Code0,E1} = compile_clause(H,E0),
        {Code1,E2} = compile_next_clause(T,E1),
	{[flatten([{clause_header,get_arity(E0)},Code0,{clause_end,Lb}])|Code1]
	 ,E2};
compile_next_clause([],E0) ->
	{[],E0}.

compile_clause({clause,Line,Head,Guard,Body},E0) ->
    {L0,L1} = var_to_save([Head,Guard,Body]),
    E1 = set_reg(set_var_to_be_saved(set_var_to_be_exported(
	     new_symbol_table(clear_allocate(set_where(E0,head))),
	     L1),L0),get_arity(E0)),
    E2 = case Guard of
	     [] -> E1;
	     _ ->  add_fail(E1)
	 end,
    dformat("compile head:~w~n",[Head],E2),
    {CodeH,E3} = compile_head(Head,E2),
    dformat("code:~w~n~w~ncompile guard:~w~n",[CodeH,Guard],E0),
    {CodeG,E4} = compile_guard(Guard,E3),
    dformat("code:~w~ncompile body:~w~n",[CodeG,Body],E0),
    E40 = check_if_options(Body,E4),
    {CodeB,E5} = compile_seq(Body,{x,0},set_where(reset_where(E40),body)),
    CodeB1 = lists:flatten(CodeB),
    % to test CodeB in gen_allocate CodeB must be flatten
    case gen_allocate(E5,CodeB1) of
	false -> {[CodeH,CodeG,CodeB1,{return}],E5};
	{N1,CodeI} -> 
	    {[CodeH,CodeG,{allocate,N1,none},CodeI,CodeB1,
	      {deallocate,N1},{return}],E5}
    end.

%______________________________________________________________________
% compile_head(Args, Env) -> {Code, Env'}.

compile_head(Args, E) ->
	match_args(Args, 0, E).

%__________________________________________________________________
%% compile_guard(Guard,Env) -> {Code, Env'}.
%%   should leave nothing extra on the stack      

compile_guard([],E) ->
	{[],E};
compile_guard([H|T],E0) ->
	dformat("compile guard element:~w~n",[H],E0),
        E1 = cond_set_reg(E0,get_arity(E0)),
        E10 = add_fail(E1),
	{Code1,E2} = compile_guard_element(H,E10),
	dformat("Guard element Code:~w~n",[Code1],E0),
	{Code2,E3} = compile_guard(T,E2),
	{[Code1,Code2],E3}.

%  when compiling a guard elemet we have to be sure we don't accidently
%  clobber anything in argument registers

compile_guard_element({op,_,Op,X,Y},E0) ->
    Flag = ret_val(E0),
    {T0,Code0,R0,_,E1} = compile_and_reuse_reg(X,set_ret_val(true,E0)),
    {T1,Code1,R1,E2} = compile_expr(Y,E1),
    % now check type of operands and optimize operations
    {[Code0,Code1,
      {test,{optimize_comp(T0,T1,Op,R0,R1,E2),Op,R0,R1},where(E0),E2}],
       set_ret_val(Flag,E2)};
compile_guard_element({call,_,{atom,_,Type},[Arg]},E0) ->
    Flag = ret_val(E0),
    E1 = case Arg of
	     {var,_,VarName} ->
		 type_var(VarName,Type,E0); % add type info
	     _ -> E0
	 end,
    {_,Code,R,E2} = compile_expr(Arg,set_ret_val(true,E1)),
    {[Code,{test,{test, Type, R}, where(E1),E2}],set_ret_val(Flag,E2)};
compile_guard_element(Other,E) ->
    throw({error, {none, ?MODULE, {compile_guard_element, Other}}}).

%______________________________________________________________________
%   compile_seq(Body,Ret,Env) -> {Code,E'}
% Ret is return value register
% if return value is not required do not generate move instruction

compile_seq([H],Ret,E0) ->
    {Type,Code1,R,E1} = compile_expr(H,E0),
    {Code2,E2} = case where(E0) of
		     body_case -> 
		       % move exported variables to local frame
			 save_exp_var(E1);
		     _ -> {[],E1}
		 end,
    case R of
	Ret -> {[Code1,Code2],E2}; % already in right register
	_ -> case Ret of
		 {x,0} -> {[Code1,Code2,{move,R,Ret}],E2};
		 _ ->
		     case ret_val(E2) of
			 true -> {[Code1,Code2,{move,R,Ret}],E2};
			 false -> {[Code1,Code2],E2} % ret val not req
		     end
	     end
    end;
compile_seq([H|T],Ret,E0) ->
    {T1,Code1,R1,E1} = compile_expr(H,E0),
    {Code2,E2} = compile_seq(T,Ret,E1),
    {[Code1,Code2],E2};
compile_seq([],_,E) ->
    {[],E}.


%______________________________________________________________________

string_to_list([H|T],L) ->
    {cons,L,{integer,L,H},string_to_list(T,L)};
string_to_list([],L) ->
    {nil,L}.


%______________________________________________________________________
% compile_case(Case, Env) -> {Type,Code,Ret,Env'}
% compile a case, reuse registers
% Ret is return value register
% save all variables to be saved before entering 'case'

compile_case({'case',Func,Options},E0) ->
    %% Flag states if the return value is required
    Flag = ret_val(E0),
    {Code1,E1} = save_var(E0),
    {_,Code2,R1,_,E2} = compile_and_reuse_reg(Func,set_ret_val(true,E1)),
    {N,E3} = new_reg(set_ret_val(Flag,E2)), % get next available reg #
    Lb = new_lb(), % prepare case ret label
    {Code3,E4} = compile_option_list(Options,R1,{x,N},Lb,'case',E3),
    {no,
     [Code1,Code2,{{'case',R1},Lb,Code3,func_name(E0)}],
     {x,N},E4}.
	
%______________________________________________________________________
% compile_if(If, Env) -> {Type,Code,Ret,Env'}
% Ret is return value register
% save all variables to be saved before entering 'if'
compile_if({'if',Options},E0) ->
        {Code1,E1} = save_var(E0),
        {N,E2} = new_reg(E1), % get next available reg #
        Lb = new_lb(), % prepare if ret label
	{Code2,E3} = compile_option_list(Options,none,{x,N},Lb,'if',E2),
	{no,
	 [Code1,{'if',Lb,Code2,func_name(E0)}],
	 {x,N},E3}.

%______________________________________________________________________
compile_receive({'receive',Line,Options,[],[]},E0) ->
    %% receive 
    %% ...
    %% end
        {Code1,E1} = save_var(E0),
        {N,E2} = new_reg(E1), % get next available reg #
        Lb0 = new_lb(), % prepare receive ret label
        Lb1 = new_lb(), % prepare receive message label
        {M,E20} = new_local_var(E2),
	{Code2,E3} = 
            compile_option_list(Options,{y,M},{x,N},Lb0,'receive',E20),
	{no,
	 [Code1,{'receive',Lb0,Lb1,Code2,{wait,Lb1},func_name(E0),M}],
	 {x,N},E3};

compile_receive({'receive',Line,[],Func,TimeOutBody},E0) ->
    %% receive after ... end.
    Flag = ret_val(E0),
    {Code1,E1} = save_var(E0),
    {_,Code2,R1,_,E2} = compile_and_reuse_reg(Func,set_ret_val(true,E1)),
    {N,E3} = new_reg(set_ret_val(Flag,E2)), % get next available reg #
    Lb0 = new_lb(), % prepare receive ret label
    Lb1 = new_lb(), % prepare receive message label
    {Code3,E4} = compile_option_list([{body,Line,TimeOutBody}],{y,0},{x,N},Lb0,
				     'receive',E3),
    {no,
     [Code1,Code2,
      {'receive',Lb0,Lb1,Code3,{waitTimeOut,Lb1,R1,func_name(E0)},
       func_name(E0),0}],
     {x,N},E4};
    
compile_receive({'receive',Line,Options,Func,TimeOutBody},E0) ->
    %% receive 
    %%   [....] when this part is absent Options = []
    %% after ...
    %% end
    Flag = ret_val(E0),
    {Code1,E1} = save_var(E0),
    {_,Code2,R1,_,E2} = compile_and_reuse_reg(Func,set_ret_val(true,E1)),
    {N,E3} = new_reg(set_ret_val(Flag,E2)), % get next available reg #
    Lb0 = new_lb(), % prepare receive ret label
    Lb1 = new_lb(), % prepare receive message label
    {M,E30} = new_local_var(E3),
    % code for TimeOutBody is treated as another part of Options
    %  it is added as the first option and taken away 
    %  while optimizing later
    {Code3,E4} = compile_option_list(
	    [{body,Line,TimeOutBody}|Options],{y,M},{x,N},Lb0,'receive',E30),
    {no,
     [Code1,Code2,
      {'receive',Lb0,Lb1,Code3,{waitTimeOut,Lb1,R1,func_name(E0)},
       func_name(E0),M}],
     {x,N},E4}.

%______________________________________________________________________
% compile_option_list(Opts,R,Ret,Lb,What,Env) -> {Code, Env'}
%%   compile an option list of an if, case or receive
% Ret is return value register
% R points to arg we do mach to
% Lb is return label
% Code is a list of lists containg code for different options
% the same vars introduced in different parts of the case option list
% must reside in the same register

compile_option_list([],_,_,_,_,E) ->
    {[],E};
compile_option_list([H|T],R,Ret,Lb,What,E0) ->
    {Code1,E1} = compile_hgb(H,R,Ret,Lb,What,E0),
    % we have to take the worse case of number of local variables
    %    generated in each option
    {Code3,E3} = compile_option_list(T,R,Ret,Lb,What,E0),
    N = larger(local_var(E1),local_var(E3)),
    M = larger(allocate(E1),allocate(E3)),
    {[lists:flatten(Code1)|Code3],set_allocate(set_local_var(E1,N),M)}.

%______________________________________________________________________
%% compile_hgb({clause,Head,Guard,Body},R,Res,LR,What,E0) -> {Code, Env'}
%%   compile head, Guard + Body in an option list
%% R points to input argument we match
%% Res is register with return value
%% LR is return label

compile_hgb({body,_,B},_,Res,LR,'receive',E0) -> % time_out body
    {CodeB,E1} = compile_seq(B,Res,set_where(E0,body_case)),
    {[{testPattern,LR,0}|CodeB],reset_where(E1)};
compile_hgb({clause,_,[],G,B},_,Res,LR,'if',E0) ->
    LP = new_next_pattern_lb(), % prepare next pattern label
    {CodeG,E1} = compile_guard(G,set_where(E0,{head_case,LP})),
    LP0 = case pattern_fail(E1) of
	      true -> LP;
	      _ -> 0
	  end,
    {CodeB,E2} = compile_seq(B,Res,set_where(reset_where(E1),body_case)),
    {[{testPattern,LR,LP0},CodeG,CodeB],reset_where(E2)};
compile_hgb({clause,_,[H],G,B},R,Res,LR,What,E0) -> % case, receive
    LP = new_next_pattern_lb(), % prepare next pattern label
    {CodeH,E1} = match(receive_head_t(H),R,set_where(E0,{head_case,LP})),
    {CodeG,E2} = compile_guard(G,E1),
    LP0 = case pattern_fail(E2) of
	      true -> LP;
	      _ -> 0
	  end,
    {CodeB,E3} = compile_seq(B,Res,set_where(reset_where(E2),body_case)),
    CodeB1 = case What of
		 'receive' -> [removeMessage|CodeB]; % receive
		 _ -> CodeB
	     end,
    {[{testPattern,LR,LP0},CodeH,CodeG,CodeB1],reset_where(E3)}.

receive_head_t({msg,X,Y}) -> {tuple,[X,Y]};
receive_head_t(X) -> X.


% the following code is used for reusing registers
% initial available register number (av_r_n) is N0
% if compiled element is 
%     var residing in reg -> reset av_r_n to N0
%     something residing in reg N1 -> reset av_r_n to N1 + 1    
%          must be done always
%     something not residing in reg -> reset av_r_n to N0
% you cannot reuse registers in head and case_head
% compile_and_reuse_reg(X,E) -> {T,Code,R,N,E'}
compile_and_reuse_reg(X,E0) ->
    {N0,_} = new_reg(E0),  % get next available reg #
    case compile_expr(X,E0) of
	{var,Code0,{x,N1},E1} ->
	    {var,Code0,{x,N1},N0,cond_set_reg(E1,N0)}; % reset next available reg #
	{T0,Code0,{x,N1},E1} ->
	    {T0,Code0,{x,N1},N0,set_reg(E1,N1+1)}; % reset next available reg #
	{T0,Code0,R0,E1} ->
	    {T0,Code0,R0,N0,cond_set_reg(E1,N0)}  % reset next available reg #
    end.


% check if the last expression in a clause is if, case, receive and thus
% its value is used as the ret value
% if yes set the ret_val flag in E to true
% check_if_options(Body,E) -> E'
check_if_options([H],E) -> 
    case H of
	{'case',_,Func,Options} -> set_ret_val(true,E);
	{'if',_,Options} -> set_ret_val(true,E);
	{'receive',_,Options,Func,TimeOutBody} -> set_ret_val(true,E);
	{'receive',_,Options} -> set_ret_val(true,E);
	{block,_,Options} -> set_ret_val(true,E);
	_ -> E
    end;
check_if_options([H|T],E) -> check_if_options(T,E);
check_if_options([],E) -> E.

%______________________________________________________________________
%% compile_expr(Rhs,Env) -> {Type,Code,Reg,Env'}
%% Reg is a register pointing to expression
%% Type = (integer | var | atom | no) type of expression
%% Code is the generated code

compile_expr(Rhs,E0) ->
    {Type,Code,R,E1} = compile_expr1(Rhs,E0),
    {Type,Code,R,E1}.

compile_expr1({nil,_},E0) ->
    {no,[],nil,E0};
compile_expr1({call,_,{atom,_,Function},Args},E0) -> % local function
    {Code00,E01} = save_arg_reg(Args,0,[],E0),
    {Code0,E1} = build_function(Args,0,E01),
    Arity = length(Args),
    Type = unknown,
    {Type,
     [Code00,Code0,
      {call,
       l_func_table(Function, Arity),	   
       Arity,
       func_name(E1),
       Function}],
     {x,0},
     reset_env(E1)};
compile_expr1({call,_,{remote,_,{atom,_,Module},{atom,_,Function}},Args},E) -> 
    compile_remote_or_bif(Module, Function, Args, E);
compile_expr1({call,_,Fun,Args},E) -> 
    compile_lambda_call(Fun, Args, E);
compile_expr1({make_fun, Lf, Free, Info}, E) -> 
    compile_make_fun(Free, Info, E);
compile_expr1({'receive',Line,Opts,Func,TimeOutBody},E0) -> 
	compile_receive({'receive',Line,Opts,Func,TimeOutBody},E0);

compile_expr1({tuple,_,[]},E0) -> 
        {N,E1} = new_reg(E0),
	{no, [mkTestHeap(1, E1), {put,{putTuple, {x,N}, []}}],{x,N},E1};
compile_expr1({tuple,_,Args},E0) -> 
    {Dest, E1} = new_reg(E0),
    {Code0, Code1, E3} = build_tuple(Args, Dest, E1),
    E4 = cond_set_reg(E3, Dest), % reset next available reg #
    Arity = length(Args),
    {no,
     [Code0, mkTestHeap(Arity+1, E4), {put,{putTuple,{x,Dest}, Code1}}],
     {x, Dest}, E4};

compile_expr1({var,Line,VarName},E0) -> 
    case lookup_symbol_table(VarName,E0) of
	{found,{{R,N},_}} -> {var,[],{R,N},E0};
	{found,{msg,_}} -> {var,[],msg,E0}; % receive
	{found,Value} -> {var,[],Value,E0};
	not_found ->
	    throw({error,{Line,?MODULE,{undefined_variable,VarName}}})
    end;

compile_expr1({string,Line,X}, E0) ->
    case length(X) of
	Length when Length < 3 ->
	    %% Short strings are better expressed as lists.
	    compile_expr(string_to_list(X, Line), E0);
	Length ->
	    {M, _} = new_reg(E0),
	    {no,
	     [mkTestHeap(2*Length, E0),
	      {put, {putString, {x, M}, Length, X}}],
	     {x, M}, E0}
    end;

compile_expr1({atom,_,A},E0) -> 
    {atom,[],{atom, A},E0};
compile_expr1({integer,Line,N},E0) -> 
    {integer,[],{i,N},E0};
compile_expr1({float,_,N},E0) -> 
    {M,_} = new_reg(E0),
    {float, [{move, N, {x,M}}], {x,M}, E0};
compile_expr1({'if',_,Options},E0) ->
    compile_if({'if',Options},E0);
compile_expr1({'receive',Line,Opts},E0) -> 
    compile_expr1({'receive',Line,Opts, [], []},E0);
compile_expr1({'catch',_,Func},E0) ->
    % catch is treated as func call, return value in x(0)
    Flag = ret_val(E0),
    {Code0,E1} = save_var(E0),

    %% Note that the expression in the catch in its turn may have a
    %% catch!  To ensure that the inner catch is found first when the
    %% emulator search for a catch, the inner catch must place its catch
    %% pointer in y register with a LOWER number than the outer catch.
    %% Therefore, allocate the y register for outer catch after
    %% compiling the inner expression.
    
    {T,Code1,R,E2} = compile_expr(Func,set_ret_val(true,E1)),
    {M,E3} = new_local_var(E2),
    Code2 = case R of
		{x,0} -> [];			% already in right register
		_ -> [{move,R,{x,0}}]
	    end,
    L = new_lb(),
    {T,
     [Code0,
      {'catch',{'catch',{y,M},L}},
      Code1,Code2,
      {'catch',{catchEnd,{y,M},L}}],
     {x,0},
     reset_env(set_ret_val(Flag,E3))};
compile_expr1({block, _, B}, E0) ->
    Flag = ret_val(E0),
    E00 = set_ret_val(true,E0), 
    {N,_} = new_reg(E00), % get next available reg #
    {CodeB,E1} = compile_seq(B,{x,N},set_where(E00,body)),
    E2 = set_ret_val(Flag,reset_where(set_reg(E1,N))),
    {no,CodeB,{x,N},E2};
compile_expr1({cons,_,H,T},E0) -> 
    %% Flag states if the return value is required
    Flag = ret_val(E0),
    {_,Code1,R1,N0,E1} = compile_and_reuse_reg(H,set_ret_val(true,E0)),
    {Code2,R2,E2} = save_reg(T,R1,E1),
    {_,Code3,R3,E3} = compile_expr(T,E2),
    E4 = cond_set_reg(set_ret_val(Flag,E3),N0), % reset next available reg#
    {no,
     [Code1,Code2,Code3, mkTestHeap(2, E4),
      {put,{putList2,{x,N0},R2,R3}}],
     {x,N0},E4};
compile_expr1({match,_,Lhs,Rhs},E0) -> 
    %% the value of match is the value of Rhs
    %% Flag states if the return value is required
    Flag = ret_val(E0),
    {T,Code1,R1,_,E1} = compile_and_reuse_reg(Rhs,set_ret_val(true,E0)),
    {Code2,E2} = match(Lhs,R1,set_ret_val(Flag,E1)),
    {Code3,E3} = save_var(E2),
    E4 = case Lhs of
	     {var,_,VarName} -> 
		 if T == float -> type_var(VarName,float,E3);
		    true -> E3
		 end;
	     _ -> E3
	 end,
    {T,[Code1,Code2,Code3],R1,E4};
compile_expr1({'case',_,Func,Options},E0) ->
    compile_case({'case',Func,Options},E0);
compile_expr1({op,_, '!', Id, Mess},E0) ->
    Args = [Id, Mess],
    {SaveCode, E1} = save_arg_reg(Args, 0, [], E0),
    {ArgsCode, E2} = build_function(Args, 0, E1),
    E3 = reset_env(E2),
    {no, [SaveCode, ArgsCode, send], {x, 0}, E3};
compile_expr1({op, Line, '++', List1, List2}, E) ->
    compile_expr1({call,Line,{remote,Line,{atom,Line,erlang},{atom,Line,append}},
		   [List1, List2]},E);
compile_expr1({op, Line, '--', List1, List2}, E) ->
    compile_expr1({call,Line,{remote,Line,{atom,Line,erlang},{atom,Line,subtract}},
		   [List1, List2]},E);
compile_expr1({op,Line,'and',X,Y},E0) ->
    compile_expr1({call,Line,{remote,Line,{atom,Line,erlang},{atom,Line,sand}},
		   [X,Y]},E0);
compile_expr1({op,Line,'or',X,Y},E0) ->
    compile_expr1({call,Line,{remote,Line,{atom,Line,erlang},{atom,Line,sor}},
		   [X,Y]},E0);
compile_expr1({op,Line,'xor',X,Y},E0) ->
    compile_expr1({call,Line,{remote,Line,{atom,Line,erlang},{atom,Line,sxor}},
		   [X,Y]},E0);
compile_expr1({op,Line,'>',X,Y},E0) ->
    compile_expr1({call,Line,{remote,Line,{atom,Line,erlang},{atom,Line,sgt}},
		   [X,Y]},E0);
compile_expr1({op,Line,'>=',X,Y},E0) ->
    compile_expr1({call,Line,{remote,Line,{atom,Line,erlang},{atom,Line,sge}},
		   [X,Y]},E0);
compile_expr1({op,Line,'<',X,Y},E0) ->
    compile_expr1({call,Line,{remote,Line,{atom,Line,erlang},{atom,Line,sgt}},
		   [Y,X]},E0);
compile_expr1({op,Line,'=<',X,Y},E0) ->
    compile_expr1({call,Line,{remote,Line,{atom,Line,erlang},{atom,Line,sge}},
		   [Y,X]},E0);
compile_expr1({op,Line,'==',X,Y},E0) ->
    compile_expr1({call,Line,{remote,Line,{atom,Line,erlang},{atom,Line,seqeq}},
		   [X,Y]},E0);
compile_expr1({op,Line,'=:=',X,Y},E0) ->
    compile_expr1({call,Line,{remote,Line,{atom,Line,erlang},{atom,Line,seq}},
		   [X,Y]},E0);
compile_expr1({op,Line,'/=',X,Y},E0) ->
    compile_expr1({call,Line,{remote,Line,{atom,Line,erlang},{atom,Line,sneqeq}},
		   [X,Y]},E0);
compile_expr1({op,Line,'=/=',X,Y},E0) ->
    compile_expr1({call,Line,{remote,Line,{atom,Line,erlang},{atom,Line,sneq}},
		   [X,Y]},E0);
compile_expr1({op,Line,'^',X1,X2},E) ->
    compile_expr1({call,Line,{remote,Line,{atom,Line,math},{atom,Line,pow}},[X1,X2]},E);
compile_expr1({op,Line,Op,X1,X2},E0) ->
    Flag = ret_val(E0),
    {T1,Code1,R1,N0,E1} = compile_and_reuse_reg(X1,set_ret_val(true,E0)),
    {Code2,R2,E2} = save_reg(X2,R1,E1),
    {T2,Code3,R3,E3} = compile_expr(X2,E2),
    E4 = cond_set_reg(set_ret_val(Flag,E3),N0), % reset next available reg#
    case eval_const_expr(Op, R2, R3, E4, Line) of
	false ->
	    {Arith,T3,R4,R5} = optimize_arith(Op,T1,T2,R2,R3,E4),
	    {T3,
	     [Code1,Code2,Code3,
	      {arith,{Arith,Op,R4,R5,{x,N0}},where(E0),E4}],
	     {x,N0},E4};
	{T3,Code4,R4,E5} ->
	    {T3,[Code1,Code2,Code3,Code4],R4,E5}
    end;
compile_expr1({op,_,'bnot',X},E0) ->
    {N0,_} = new_reg(E0), % get next available reg #
    Flag = ret_val(E0),
    {_,Code1,R1,E1} = compile_expr(X,set_ret_val(true,E0)),
    E2 = cond_set_reg(set_ret_val(Flag,E1),N0), % reset next available reg#
    {no,
     [Code1,{arith,{arithBnot,R1,{x,N0}},where(E0),E2}],
     {x,N0},E2};
compile_expr1({op,Line,'-',X}, E) ->
    compile_expr1({op,Line,'-',{integer,Line,0},X},E);
compile_expr1({op,_,'+',X}, E) ->
    compile_expr(X, E);
compile_expr1({op,Line,'not',X}, E) ->
    compile_expr1({call,Line,{remote,Line,{atom,Line,erlang},{atom,Line,snot}},
		   [X]},E);

compile_expr1(Other,_) ->
    throw({error, {none, ?MODULE, {compile_expr1,Other}}}).


compile_remote_or_bif(erlang, Func, Args, E) ->
    Arity = length(Args),
    case guard_bif(Func, Arity) of
	false ->
	    compile_remote_call(erlang, Func, Args, Arity, E);
	true ->
	    compile_bif(Func, Args, Arity, E)
    end;
compile_remote_or_bif(Mod, Func, Args, E) ->
    compile_remote_call(Mod, Func, Args, length(Args), E).

compile_bif(Func, Args, Arity, E0) ->
    {Code0, ArgsReg, E1} = build_bif(Args, false, E0),
    {N0, E2} = new_reg(E1),			% get next available reg #
    {unknown,
     [Code0,
      {bif,{Func, ArgsReg, {x, N0}}, where(E0), E2}],
     {x, N0}, E2}.

compile_remote_call(Module, Function, Args, Arity, E0) ->
    {Code00,E01} = save_arg_reg(Args,0,[],E0),
    {Code0,E1} = build_function(Args,0,E01),
    {no,
     [Code00,Code0,
      {callEx,
       please_remove_me,			% XXX Temporary filler.
       Module, Function, Arity, func_name(E0)}],
     {x,0},reset_env(E1)}.


compile_lambda_call(Fun, Args, E0) ->
    Arity = length(Args),
    AllArgs = Args ++ [Fun],
    {Code0, E1} = save_arg_reg(AllArgs, 0, [], E0),
    {Code1, E2} = build_function(AllArgs, 0, E1),
    {no, [Code0, Code1, {call_fun, Arity}], {x,0}, reset_env(E2)}.

compile_make_fun(Free, {Func, Arity, Index, Uniq}, E0) ->
    {SaveCode, E1} = save_arg_reg(Free, 0, [], E0),
    {LoadCode, E2} = build_function(Free, 0, E1),
    MakeFun = {make_fun, {f,l_func_table(Func,Arity)}, Uniq, length(Free)},
    {no, [SaveCode, LoadCode, MakeFun], {x,0}, reset_env(E2)}.

%_____________________________________________________________________
%% match_args(List, Start, Env) -> {Code,Env'}.
%%   match elements in List in consequtive Arg registers starting at Start
%%   you cannot reuse registers due to common subexpression elimination

match_args([H|T], N, E0) ->
     %io:format("~nEnv0: ~w~n", [E0]),
    {CodeH,E2} = match(H,{x,N},E0),
    %io:format("~nEnv1: ~w~n", [E2]),
    {CodeT,E3} = match_args(T,N + 1,E2),
    {[CodeH,CodeT],E3};
match_args([],_,E) ->
    {[],E}.

%______________________________________________________________________
%% match_in_tuple(L, ArgN, R, Env) -> {Code,Env'}
%%   match in a tuple
%% L - list of tuple elements
%% ArgN - which element (0,1,...)
%% R - register containing the tuple

match_in_tuple([{var,_,'_'}|T],ArgN,R,E)->
        match_in_tuple(T,ArgN + 1,R,E);
match_in_tuple([H|T],ArgN,R,E0)->
        {M,E1} = new_reg(E0),
        {Code0,E2} = match(H,{x,M},E1),
        E3 = cond_set_reg(E2,M), % reset next available reg #
        {Code1,E4} = match_in_tuple(T,ArgN + 1,R,E3),
	{[{get,{getTupleElement,R,{x,M},ArgN}},Code0,Code1],E4};
match_in_tuple([],_,_,E) ->
        {[],E}.

%__________________________________________________________________
%% match(Struct,R,E) -> {Code,Env'}.
%%   match Struct with R
%%   all matching done in registers

match({nil,_},R,E) ->
    {[{test,{test,nil,R},where(E),E}],add_fail(E)};
match({atom,_,X},R,E) ->
    {[{test,{equal_atom, R, {atom, X}},where(E),E}],
     add_fail(E)};
match({var,_,'_'},_,E) ->
    {[],E};

% the following introduces new values in var symbol table: nil, {atom,N}, {i,N}
% is called from compile match
match({match,_,Left,Right},Y,E0) ->
    {CR, E1} = match(Right,Y,E0),
    {CV, E2} = match(Left,Y,E1),
    {[CR,CV],E2};
match({var,_,VarName},Y,E0) ->
    case insert_symbol_table(VarName,Y,E0) of
	{ok,E1} -> {[],E1};
	{conflict,Value,T} ->
	    E1 = add_fail(E0),
	    Opt = case Y of
		      {i,N} ->
			  optimize_comp(integer,T,'=:=',Y,Value,E1);
		      {atom, _} -> 
			  optimize_comp(atom,T,'=:=',Y,Value,E1);
		      _ -> comp
		  end,
	    {[{test,{Opt,'=:=',Y,Value},where(E0),E1}],E1}
    end;
match({integer,Line,X},R,E0) ->
    {[{test,{equal_int,R,X},where(E0),E0}], add_fail(E0)};
match({float,_,X},R,E0) ->
    {M,E1} = new_reg(E0),
    {[{test,{test,float,R},where(E0),E0},
      {move, X, {x,M}},
      {test,{comp,'=:=',R,{x,M}},where(E0),E1}],
     add_fail(E1)};
match({tuple,_,Args},R,E0) ->
    {Code,E1} = match_in_tuple(Args,0,R,E0),
    {[{test,{test,tuple,R},where(E0),E0},
      {test,{testTupleArity,R,length(Args)},where(E0),E0}|Code],
     add_fail(E1)};
match({string,Line,X},R,E) ->
    match(string_to_list(X,Line),R,E);
match({cons,_,H,T},R0,E0) ->
    {R1,E1} = case can_be_reused(R0,E0) of
		  false -> {M0,E01} = new_reg(E0), {{x,M0},E01};
		  true -> {R0,E0}
	      end,
    {M1,E2} = new_reg(E1),
    {Hcode,E3} = match(H,R1,E2),
    E4 = cond_set_reg(E3,M1+1), % reset next available reg #
    {Tcode,E5} = match(T,{x,M1},E4),
    {[{test,{test,nonEmptyList,R0},where(E0),E0},
      {get,{getList2,R0,R1,{x,M1}}},Hcode,Tcode],
     add_fail(E5)};
match(Other,_,_) ->
    throw({error, {none, ?MODULE,{match,Other}}}).


%______________________________________________________________________
% build_function(Args,N,E) -> {Code,E'}
% evaluate arguments, load argument registers
% N is current number of argument register
% before each move instruction we move some var to local frame
build_function([H|T],N,E0) ->
    {Code0,E1} = save_var(E0),
    E2 = set_reg(E1,N),
    Flag = ret_val(E2),
    {_,Code1,R0,E3} = compile_expr(H,set_ret_val(true,E2)),
    {Code2,R1,E4} = save_reg(T,R0,set_ret_val(Flag,E3)),
    {Code3,E5} = build_function(T,N+1,E4),
    case R1 of
	{x,N} -> % already in right register
	    {[Code0,Code1,Code2,Code3],E5};
	{x,_} ->
	    {[Code0,Code1,Code2,[{move,R1,{x,N}}],Code3],E5};
	_ -> % make move just before call
	    {[Code0,Code1,Code2,Code3,{move,R1,{x,N}}],E5}
    end;
build_function([],_,E) ->
    save_var(E).

%______________________________________________________________________
% build_bif(Args,Gc,E) -> {Code,ArgsReg,E'}
% evaluate arguments
% Gc = (false | true) bif doing GC
% ArgsReg = list with Reg containing arguments
% we do save_var each time we call to save new introduced var
build_bif([H|T],Gc,E0) ->
    {Code0,E1} = if 
		     Gc == true -> save_var(E0);
		     true -> {[],E0}
		 end,
    Flag = ret_val(E1),
    {_,Code1,R1,_,E2} = compile_and_reuse_reg(H,set_ret_val(true,E1)),
    {Code2,R2,E3} = save_reg(T,R1,set_ret_val(Flag,E2)),
    {Code3,ArgsReg,E4} = build_bif(T,Gc,E3),
    {[Code0,Code1,Code2,Code3],[R2|ArgsReg],E4};
build_bif([],true,E0) ->
    {Code,E1} = save_var(E0),
    {Code,[],E1};
build_bif([],_,E) ->
    {[],[],E}. 


%______________________________________________________________________
% build_tuple([H|T],N0,E0) 
% N0 is last used register
% returns {Code0, Code1, E'}
% Code0 compiled expressions, Code1 tuple elements, 

build_tuple([H|T],N0,E0) ->
    Flag = ret_val(E0),
    {Type,Code1,Reg,E1} = compile_expr(H,set_ret_val(true,E0)),
    {R1,N2,Code2,E2} = case {Type,Reg} of
		  {var,{x,N1}} ->
		      {{x,N1},N0,[],E1};
		  {_,{x,N1}} when N1 =< N0 ->
		  % N1 can be 0 if H is a function call
                  % N2 must be larger than N0
		  % save result in local frame
		      {N3,E10} = new_local_var(E1),
		      {{y,N3},N0,[{move,Reg,{y,N3}}],E10};
		  {_,{x,N1}} ->
		      {{x,N1},N1,[],E1};
		  {_,R0} ->
		      {R0,N0,[],E1}
	      end,
    {Code3,R2,E3} = save_reg(T,R1,set_ret_val(Flag,E2)),
    E4 = set_reg(E3,N2+1), % reset next available reg #
    {Code4,Code5,E5} = build_tuple(T, N2, E4),
    {[Code1,Code2,Code3,Code4], [R2|Code5], E5};
build_tuple([], _, E) ->
    {[], [], E}.

%______________________________________________________________________
%% dformat(Format,Data,Env)  -> true
%%   debug info (printed if debug_flag is true

dformat(Format, Data, E) -> 
	case debug_flag(E) of
	    true  -> io:format(Format, Data);
	    false -> true
	end.


% gen_allocate(E0,Body) -> ({Size,Code} | false)
% check if generate local frame
% Code = code to initialize local frame
% if there is only one func call at the end generate CallOnly without Allocate
% for threaded code the number of local variables is set from the begining to 1
%    if there are function calls

gen_allocate(E,Body) ->
    N0 = local_var(E),
    N1 = allocate(E),
    if
	N0 == 0 -> 
	    if 
		N1 == 0 ->
		    false;
		N1 == 1 -> 
		    case lists:last(Body) of
			{call,Line,_,_,_} ->
			    false;
			_ ->
			    {0, []}
		    end;
		true ->
		    {0,[]}
	    end;
	N0 == 1 ->
	    case var_to_be_exported(E) of
		[0] -> % no local var (threaded code)
		    if 
			N1 == 0 ->
			    false;
			N1 == 1 -> 
			    case lists:last(Body) of
				{call,Line,_,_,_} ->
				    false;
				_ ->
				    {N0,gen_init(N0,[])}
			    end;
			true ->
			    {N0,gen_init(N0,[])}
		    end;		    
		_ ->
		    {N0,gen_init(N0,[])}
	    end;
	true ->
	    {N0,gen_init(N0,[])}
    end.

% gen_init(N) -> Code
% generate Code for initialization of local frame
% the Code is reversed to be optimized later with Move instructions
gen_init(0, L) -> reverse(L);
gen_init(N, L) -> 
    gen_init(N-1, [{init, N-1}|L]).

%% var_to_save(Form) -> {SaveInFrame, Exported}.
%%   Form = The abstract parse tree from the parser.
%%   SaveInFrame = List of variables to be saved in stack frame for function
%%		   (thus, they will become y variables).  All variables used
%%		   across calls or receive/case/if.
%%   Exported = List of variables which are assigned inside a receive/case/if
%%		and used afterwards.  (Note: Erl_lint has already checked
%%		that each of these variables is assigned in every branch
%%		of receive/case/if.)

var_to_save(Form) ->
    L = var_calls(Form),
    {var_to_save(remove_opt(L, []), [], []),
     var_to_export(remove_call(L, []), 0, [], [], [])}.

%% Remove options info from var_calls list -> new_list (and reverse).

remove_opt([options|T],L) -> remove_opt(T,L);
remove_opt([opt_end|T],L) -> remove_opt(T,L);
remove_opt([H|T],L) -> remove_opt(T,[H|L]);
remove_opt([],L) -> reverse(L).

%% Remove calls from var_calls list -> new_list (and reverse).

remove_call([call|T],L) -> remove_call(T,L);
remove_call([H|T],L) -> remove_call(T,[H|L]);
remove_call([],L) -> L.


% Remove duplicates from list.
remove_dupl(L) ->
    remove_dupl(L, []).

remove_dupl([H|T], L) ->
    case member(H, L) of
	true -> remove_dupl(T, L);
	false -> remove_dupl(T, [H|L])
    end;
remove_dupl([], L) ->
    reverse(L).

%% var_to_save(L0, V, L1) -> VarsToSave  [helper for var_to_save/1]
%%   L0 = List of used of variables and calls.
%%   V  = Temporary list of variables between calls.
%%   VarsToSave = List of variables to save (because they are used across calls
%%		or receive/case/if).

var_to_save([], V, L) ->
    remove_dupl(L, []);
var_to_save([H], V, L) ->
    remove_dupl(L, []);
var_to_save([call|T], V, L) ->
    var_to_save(T, [], L);
var_to_save([H, call|T], [], L) ->
    case member(H, T) of
	true -> var_to_save(T, [], [H|L]);
	false -> var_to_save(T, [], L)
    end;
var_to_save([H0, call|T0], [H1|T1], L) ->
    case member(H0, T0) of
	true -> var_to_save([H1,call|T0],T1,[H0|L]);
	false -> var_to_save([H1,call|T0],T1,L)
    end;
var_to_save([X,Y|T],V,L) ->
    var_to_save([Y|T],[X|V],L).


% var_to_export(L0, N, V, R, L1) -> L1
% L0 = list of var and options info
% N  = number of current nested options lists
% V  = list of var inside options
% R  = list of var outside options
% L1 = list of var to be exported
%
% In R3 the following was true:
%   when threaded code -> the first element 
%     in the list is 0 to make place for program counter I in local frame, 
%     only if M > 0

var_to_export([],_,_,_,L) ->
    remove_dupl(L);
var_to_export([H],_,_,_,L) -> 
    remove_dupl(L);
var_to_export([options|T],0,V,R,L) ->
    var_to_export(T,1,[],R,L);
var_to_export([options|T],N,V,R,L) ->
    var_to_export(T,N+1,V,R,L);
var_to_export([opt_end|T],N,V,R,L) ->
    var_to_export(T,N-1,V,R,L);

var_to_export([H,opt_end|T],N,[],R,L) ->
    case member(H, T) of
	true ->
	    case member(H, R) of
		false -> var_to_export(T,N-1,[],R,[H|L]);
		true -> var_to_export(T,N-1,[],R,L)
	    end;
	false ->
	    var_to_export(T,N-1,[],R,L)
    end;
var_to_export([H0,opt_end|T0],N,[H1|T1],R,L) ->
    case member(H0,T0) of
	true ->
	    case member(H0,R) of
		false -> var_to_export([H1,opt_end|T0],N,T1,R,[H0|L]);
		true -> var_to_export([H1,opt_end|T0],N,T1,R,L)
	    end;
	false ->
	    var_to_export([H1,opt_end|T0],N,T1,R,L)
    end;
var_to_export([H|T],0,V,R,L) ->
    var_to_export(T,0,V,[H|R],L);
var_to_export([X,Y|T],N,V,R,L) ->
    var_to_export([Y|T],N,[X|V],R,L).

%%% var_calls(Form) -> CallsAndVars
%%   Form = The abstract form returned by the parser
%%   List = Reversed list of code reduced to used variables, function calls,
%%	and 'options' and 'opt_end' to mark beginning and end of receive/case/if.
%%   
%%   Note: Variables can be included more than once.
%%   receive/case/if are treated as calls.

var_calls(Form) ->
    var_calls(Form, []).

var_calls({nil, _},L) ->
    L;
var_calls(nil, L) ->
    L;
var_calls([], L) ->
    L;
var_calls([H|T],L) ->
    L0 = var_calls(H, L),
    var_calls(T, L0);

%% The following BIFs are defined in module erlang.
var_calls({call,_,{remote,_,{atom,_,erlang},{atom,_,Func}},Args},L) ->
    Arity = length(Args),
    case guard_bif(Func, Arity) of
	false ->
	    L0 = var_calls(Args, L),
	    [call|L0];
	true ->
	    var_calls(Args, L)
    end;
var_calls({call,_,{remote,_,{atom,_,Mod},{atom,_,Func}},Args},L) ->
    L0 = var_calls(Args, L),
    [call|L0];
var_calls({call, Line, {atom,_,_}, Args}, L) ->
    L0 = var_calls(Args, L),
    [call|L0];
var_calls({call, Line, F, Args}, L) ->
    %% This is a call to a fun.
    L0 = var_calls(Args++[F], L),
    [call|L0];
var_calls({make_fun, Line, Free, {Name,Ar,I,Uniq}}, L0) ->
    L = var_calls(Free, L0),
    [call|L];
var_calls({comp,_,_,X1,X2},L) ->
    L0 = var_calls(X1, L),
    var_calls(X2, L0);
var_calls({clause,_,H,G,B},L) ->
    var_calls(append([H,G,B]),L);
var_calls({'receive',_,Options,Func,TimeOutBody},L) ->
    L0 = var_calls(Func, [call|L]),
    L1 = var_calls(Options, [options|L0]),
    L2 = var_calls(TimeOutBody, L1),
    [opt_end|L2];
var_calls({test,_,_,[Arg]}, L) ->
    var_calls(Arg, L);
var_calls({cons,_,H,T},L) ->
    L0 = var_calls(H, L),
    var_calls(T, L0);
var_calls({match,_,Lhs,Rhs},L) ->
    var_calls(Lhs, var_calls(Rhs, L));
var_calls({'case',_,Func,Options}, L) ->
    L0 = var_calls(Func,[call|L]),
    L1 = var_calls(Options,[options|L0]),
    [opt_end|L1];
var_calls({clause,_,G,B},L) ->
    var_calls(G ++ B, L);
var_calls({op, Line, '<', X, Y}, L) ->
    %% The operands will be swapped later!
    [call| var_calls(X, var_calls(Y, L))];
var_calls({op, Line, Op, X, Y},L) ->
    case op_is_call(Op) of
	true ->
	    [call| var_calls(Y, var_calls(X, L))];
	false ->
	    var_calls(Y, var_calls(X, L))
    end;
var_calls({op, Line, Op, X},L) ->
    case op_is_call(Op) of
	true ->
	    [call| var_calls(X, L)];
	false ->
	    var_calls(X, L)
    end;
var_calls({tuple,_,Args},L) ->
    var_calls(Args,L);
var_calls({var,_,'_'},L) ->
    L;
var_calls({var,_,VarName}, L) ->
    [VarName|L];
var_calls({string,_,_}, L) ->
    L;
var_calls({atom,_,_},L) ->
    L;
var_calls({integer,_,_},L) ->
    L;
var_calls({float,_,_},L) ->
    L;
var_calls({'if',_,Options},L) ->
    L0 = var_calls(Options,[options,call|L]),
    [opt_end|L0];
var_calls({'receive',_,Options},L) ->
    L0 = var_calls(Options,[options,call|L]),
    [opt_end|L0];
var_calls({'catch',_,Func},L) ->
    L0 = var_calls(Func,L),
    [call|L0];
var_calls({block,_,B},L) ->
    var_calls(B, L);
var_calls(Other,L) ->
    throw({error, {none, ?MODULE, {var_calls,Other}}}).


%% Returns true if the given operation is technically a call (meaning
%% that the result will be returned in r(0) and other x registers destroyed);
%% otherwise false.

op_is_call('++') ->  true;
op_is_call('--') ->  true;
op_is_call('^')  ->  true;
op_is_call('!')  ->  true;
op_is_call('and') -> true;
op_is_call('or') ->  true;
op_is_call('xor') -> true;
op_is_call('not') -> true;
op_is_call('>') ->   true;
op_is_call('>=') ->  true;
op_is_call('<') ->   true;
op_is_call('=<') ->  true;
op_is_call('==') ->  true;
op_is_call('=:=') -> true;
op_is_call('/=') ->  true;
op_is_call('=/=') -> true;
op_is_call(_) ->
    false.

% save_var(E) -> {Code,E'}
% save some local var in local frame
save_var(E) ->
    save_var(symbol_table(E), var_to_be_saved(E), var_to_be_exported(E), E).

% save_var(Table, VarList, ExpVar, E) -> {Code,E'}
% save var from Table which are in VarList or ExpVar in local frame
% var in ExpVar must be saved at the position corresponding to their place
%    in the list e.g. [X,Z,T] -> Z must be saved as the second local var
% generate Code to do this
% var in VarList must reside in registers x(N) to be saved
% all var in ExpVar must be saved

save_var([],_,_,E) ->
    {[],E};
save_var(Table, Vars, ExpVar, E) ->
    save_var(Table,Vars,ExpVar,[],Table,E).

save_var([{Key,{{x,M},Type}}|T],Vars,ExpVar,L,Table,E0) ->
    case index(Key,ExpVar) of
	-1 -> 
	    case member(Key,Vars) of
		true -> 
		    {N,E1} = new_local_var(E0),
		    save_var(T,
			     Vars,
			     ExpVar,
			     [{move,{x,M},{y,N}}|L],
			     table_insert(Key,{{y,N},Type},Table),
			     E1);
		false -> save_var(T,Vars,ExpVar,L,Table,E0)
	    end;
	N -> % exported var
	    save_var(T,
		     Vars,
		     ExpVar,
		     [{move,{x,M},{y,N}}|L],
		     table_insert(Key,{{y,N},Type},Table),
		     E0)
    end;
save_var([{Key,{x,M}}|T],Vars,ExpVar,L,Table,E0) ->
    case index(Key,ExpVar) of
	-1 -> 
	    case member(Key,Vars) of
		true -> 
		    {N,E1} = new_local_var(E0),
		    save_var(T,
			     Vars,
			     ExpVar,
			     [{move,{x,M},{y,N}}|L],
			     table_insert(Key,{y,N},Table),
			     E1);
		false -> save_var(T,Vars,ExpVar,L,Table,E0)
	    end;
	N -> % exported var
	    save_var(T,
		     Vars,
		     ExpVar,
		     [{move,{x,M},{y,N}}|L],
		     table_insert(Key,{y,N},Table),
		     E0)
    end;
save_var([_|T],Vars,ExpVar,L,Table,E) ->
    save_var(T,Vars,ExpVar,L,Table,E);
save_var([],_,_,Code,Table,E) -> {Code,save_symbol_table(Table,E)}.


% save_exp_var(E) -> {Code,E'}
% save exported var in local frame
save_exp_var(E0) ->
    {Code1,E1} = save_var(E0),
    {Code2,E2} = save_exp_var(symbol_table(E1),var_to_be_exported(E1),E1),
    {[Code1,Code2],E2}.


% save_exp_var(Table, ExpVar, E) -> {Code,E'}
% save var from Table which are in ExpVar in local frame
% var in ExpVar must be saved at the position corresponding to their place
%    in the list e.g. [X,Z,T] -> Z must be saved as the second local var
% generate Code to do this
% var residing in registers x(N) have been saved by save_var

save_exp_var([],_,E) ->
    {[],E};
save_exp_var(Table,ExpVar,E) ->
    save_exp_var(Table,ExpVar,[],Table,E).


save_exp_var([{Key,Value}|Tail],ExpVar,L,Table,E) ->
    % other values of var in symbol table can be the following:
    % {y,N} | {{y,N},Type} | nil | {{atom,N},atom} | {{i,N},integer} | msg
    % {x,N} | {{x,N},Type} were already saved by save_var/4
    % it can happen that through assignment var Key has already
    % other {y,M} value than required in list ExpVar
    case index(Key,ExpVar) of
	-1 -> 
	    save_exp_var(Tail,ExpVar,L,Table,E);
	N -> % exported var
	    case Value of
		% if var was moved earlier to local frame
		% the position was the right one
		{y,N} ->
		    save_exp_var(Tail,ExpVar,L,Table,E);
		{{y,N},_} ->
		    save_exp_var(Tail,ExpVar,L,Table,E);
		{y,M} ->
		    save_exp_var(Tail,
			     ExpVar,
			     [{move,{y,M},{y,N}}|L],
			     table_insert(Key,{y,N},Table),
			     E);
		{{S,T},Type} ->
		    save_exp_var(Tail,
			     ExpVar,
			     [{move,{S,T},{y,N}}|L],
			     table_insert(Key,{{y,N},Type},Table),
			     E);
		nil ->
		    save_exp_var(Tail,
			     ExpVar,
			     [{move,nil,{y,N}}|L],
			     table_insert(Key,{y,N},Table),
			     E);
		msg ->
		    save_exp_var(Tail,
			     ExpVar,
			     [{move,msg,{y,N}}|L],
			     table_insert(Key,{y,N},Table),
			     E);
		Other ->
		    throw({error,{none,?MODULE,
				  {other_object_in_symbol_table,Key,N,Other}}})
	    end
    end;
save_exp_var([],_,Code,Table,E) -> {Code,save_symbol_table(Table,E)}.


%%% save_reg(Form,R,E) -> {Code,R',E'}
%%   Form = the abstract form returned by the parser
%%   if Form contains func calls save reg R in local frame
%%   generate Code to do this
%%   R' is the new R
save_reg(_,{y,N},E) -> {[],{y,N},E};
save_reg(_,{atom, N},E) -> {[],{atom, N},E};
save_reg(_,{i,N},E) -> {[],{i,N},E};
save_reg(_,nil,E) -> {[],nil,E};
save_reg(Form,R,E0) ->
    case save_reg(Form) of
	true -> 
	    ExpVar = var_to_be_exported(E0),
	    Table = symbol_table(E0),
	    case check_value(Table,R) of
		{Key} -> 
		    {N,E1} = case index(Key,ExpVar) of
				 -1 ->
				     new_local_var(E0); 
				 N0 ->
				     {N0,E0}
			     end,
		    E2 = save_symbol_table(table_insert(Key,{y,N},Table),E1),
		    {[{move,R,{y,N}}],{y,N},E2};
		{Type,Key} ->
		    {N,E1} = case index(Key,ExpVar) of
				 -1 ->
				     new_local_var(E0);
				 N0 ->
				     {N0,E0}
			     end,
		    E2 = save_symbol_table(
				      table_insert(Key,{{y,N},Type},Table),E1),
		    {[{move,R,{y,N}}],{y,N},E2};
		not_found -> 
		    {N,E2} = new_local_var(E0),
		    {[{move,R,{y,N}}],{y,N},E2}
	    end;
	false -> {[],R,E0}
    end.


%% save_reg(Form) -> (true | false)
%% check if Form contains func calls
save_reg({nil,_}) -> false;
save_reg(nil) -> false;
save_reg([]) -> false;
save_reg([H|T]) ->
    case save_reg(H) of
	false -> save_reg(T);
	true -> true
    end;
save_reg({call,_,{remote,_,{atom,_,erlang},{atom,_,Func}},Args}) -> %  bif
    Arity = length(Args),
    case guard_bif(Func, Arity) of
	true -> save_reg(Args);
	false -> true
    end;
save_reg({call,_,_,_}) ->
    true;
save_reg({make_fun,Line,Free,Info}) ->
    true;
save_reg({op,_,'++',_,_}) ->
    true;
save_reg({op,_,'--',_,_}) ->
    true;
save_reg({op,_,'^',_,_}) ->
    true;
save_reg({op,_,'!',_,_}) ->
    true;

% new functions in module erlang
save_reg({op,Line,'and',_,_}) ->
    true;
save_reg({op,Line,'or',_,_}) ->
    true;
save_reg({op,Line,'xor',_,_}) ->
    true;
save_reg({op,Line,'not',_}) ->
    true;
save_reg({op,Line,'>',_,_}) ->
    true;
save_reg({op,Line,'>=',_,_}) ->
    true;
save_reg({op,Line,'<',_,_}) ->
    true;
save_reg({op,Line,'=<',_,_}) ->
    true;
save_reg({op,Line,'==',_,_}) ->
    true;
save_reg({op,Line,'=:=',_,_}) ->
    true;
save_reg({op,Line,'/=',_,_}) ->
    true;
save_reg({op,Line,'=/=',_,_}) ->
    true;

save_reg({op,_,_,X1,X2}) -> % arithmetic
    case save_reg(X1) of
	false -> save_reg(X2);
	true -> true
    end;
save_reg({op,_,_,X}) -> % arithmetic
    save_reg(X);

save_reg({comp,_,_,X1,X2}) ->
        case save_reg(X1) of
	    false -> save_reg(X2);
	    true -> true
	end;
save_reg({clause,_,H,G,B}) ->
        save_reg(append([H,G,B]));
save_reg({'receive',_,Options,Func,TimeOutBody}) ->
        case save_reg(Options) of
	    false -> 
		case save_reg(Func) of
		    false -> save_reg(TimeOutBody);
		    true -> true
		end;
	    true -> true
	end;
save_reg({test,_,_,[Arg]}) ->
        save_reg(Arg);
save_reg({cons,_,H,T}) ->
        case save_reg(H) of
	    false -> save_reg(T);
	    true -> true
	end;
save_reg({match,_,Lhs,Rhs}) ->
%        case save_reg(Lhs) of
%	    false -> save_reg(Rhs);
%	    true -> true
%	end;
        true;
save_reg({'case',_,Func,Options}) ->
%        case save_reg(Func) of
%	    false -> save_reg(Options);
%	    true -> true
%	end;
        true;
save_reg({clause,_,G,B}) ->
        save_reg(append(G,B));
save_reg({tuple,_,Args}) ->
        save_reg(Args);
save_reg({var,_,_}) -> false;
save_reg({string,_,_}) -> false;
save_reg({atom,_,_}) -> false;
save_reg({integer,_,_}) -> false;
save_reg({float,_,_}) -> false;
save_reg({'if',_,Options}) ->
%	save_reg(Options);
        true;
save_reg({'receive',_,Options}) ->
%	save_reg(Options);
        true;
save_reg({'catch',_,Func}) ->
%        save_reg(Func);
        true;
save_reg({block,_,B}) ->
        save_reg(B);
save_reg(Other) ->
        throw({error,{none,?MODULE,{save_reg,Other}}}).

% reset_env(E) -> E'
% done after function calls
% reset var symbol table, add allocate, reset next available reg
reset_env(E) ->
        set_reg(add_allocate(reset_var(E)),1).


% reset_var(Table) -> E'
% remove all variables in var symbol table residing in {x,N}
reset_var(E) ->
    Table = symbol_table(E),
    reset_var(Table,[],E).

reset_var([{Key, {{x,_},Type}}|T], NewTable, E) -> 
    reset_var(T, NewTable, E);
reset_var([{Key, {x,_}}|T], NewTable, E) -> 
    reset_var(T, NewTable, E);
reset_var([H|T], NewTable, E) -> 
    reset_var(T, [H|NewTable], E);
reset_var([], NewTable, E) -> 
    save_symbol_table(NewTable,E).


% index(Element,List) -> N
% return index of Element in List
% the first element has index 0
% -1 if not found
index(E,L) -> index(E,L,0).

index(_,[],_) -> -1;
index(H,[H|T],N) -> N;
index(H0,[H1|T],N) -> index(H0,T,N+1).

larger(N0,N1) when N0 > N1 -> N0;
larger(N0,N1) -> N1.

% while doing function call and loading new argument registers
% save current x(N) registers which are destroyed by loading,
% save only those which are needed to evaluate the new arguments
%     do not save variables which reside in proper argument registers 
%     and are used later to evaluate next arguments
% save_arg_reg(Args,N,Code,E) -> {Code',E'}
save_arg_reg([Arg|T],N,Code0,E0) -> 
%   io:format("Args: ~w~n~n",[H|T]), %BH
    {Code1,E1} = save_arg_reg0(T,N,Arg,Code0,E0),
    save_arg_reg(T,N+1,Code1,E1);
save_arg_reg([],_,Code,E) -> {Code,E}.

save_arg_reg0({nil,_},_,_,Code,E) -> {Code,E};
save_arg_reg0([],_,_,Code,E) -> {Code,E};
save_arg_reg0([H|T],N,Arg,Code0,E0) ->
    {Code1,E1} = save_arg_reg0(H,N,Arg,Code0,E0),
    save_arg_reg0(T,N,Arg,Code1,E1);
save_arg_reg0({call,_,_,Args},N,Arg,Code,E) ->
    save_arg_reg0(Args,N,Arg,Code,E);
save_arg_reg0({make_fun,Line,Free,Info},N,Arg,Code,E) ->
    save_arg_reg0(Free,N,Arg,Code,E);

save_arg_reg0({op,_,_,X1,X2},N,Arg,Code,E) ->
    save_arg_reg0([X1,X2],N,Arg,Code,E);
save_arg_reg0({op,_,_,X},N,Arg,Code,E) ->
    save_arg_reg0(X,N,Arg,Code,E);

save_arg_reg0({comp,_,_,X1,X2},N,Arg,Code,E) ->
    save_arg_reg0([X1,X2],N,Arg,Code,E);
save_arg_reg0({clause,_,H,G,B},N,Arg,Code,E) ->
    save_arg_reg0([H,G,B],N,Arg,Code,E);
save_arg_reg0({'receive',_,Options,Func,TimeOutBody},N,Arg,Code,E) ->
    save_arg_reg0([Options,Func,TimeOutBody],N,Arg,Code,E);
save_arg_reg0({test,_,_,[Args]},N,Arg,Code,E) ->
    save_arg_reg0(Args,N,Arg,Code,E);
save_arg_reg0({cons,_,H,T},N,Arg,Code,E) ->
    save_arg_reg0([H,T],N,Arg,Code,E);
save_arg_reg0({match,_,Lhs,Rhs},N,Arg,Code,E) ->
    save_arg_reg0([Lhs,Rhs],N,Arg,Code,E);
save_arg_reg0({'case',_,Func,Options},N,Arg,Code,E) ->
    save_arg_reg0([Func,Options],N,Arg,Code,E);
save_arg_reg0({clause,_,G,B},N,Arg,Code,E) ->
    save_arg_reg0([G,B],N,Arg,Code,E);
save_arg_reg0({tuple,_,Args},N,Arg,Code,E) ->
    save_arg_reg0(Args,N,Arg,Code,E);
save_arg_reg0({var,_,VarName},N,Arg,Code,E0) ->
    case Arg of
	{var,_,VarName} ->
	    {Code,E0};
	_ ->
	    Table = symbol_table(E0),
	    case table_lookup(VarName,Table) of
		{found, {{x,N},Type}} -> 
		    {M,E1} = new_local_var(E0),
		    {[{move,{x,N},{y,M}}|Code],
		     save_symbol_table(table_insert(VarName,{{y,M},Type},Table),E1)};
		{found, {x,N}} ->
		    {M,E1} = new_local_var(E0),
		    {[{move,{x,N},{y,M}}|Code],
		     save_symbol_table(table_insert(VarName,{y,M},Table),E1)};
		_ -> {Code,E0}
	    end
    end;
save_arg_reg0({string,_,_},_,_,Code,E) -> {Code,E};
save_arg_reg0({atom,_,_},_,_,Code,E) -> {Code,E};
save_arg_reg0({integer,_,_},_,_,Code,E) -> {Code,E};
save_arg_reg0({float,_,_},_,_,Code,E) -> {Code,E};
save_arg_reg0({'catch',_,Func},N,Arg,Code,E) ->
    save_arg_reg0(Func,N,Arg,Code,E);
save_arg_reg0({block,_,B},N,Arg,Code,E) ->
    save_arg_reg0(B,N,Arg,Code,E);
save_arg_reg0({'if',_,Options},N,Arg,Code,E) ->
    save_arg_reg0(Options,N,Arg,Code,E);
save_arg_reg0({'receive',_,Options},N,Arg,Code,E) ->
    save_arg_reg0(Options,N,Arg,Code,E);
save_arg_reg0(Other,_,_,_,_) ->
    throw({error,{none,?MODULE,{save_arg_reg0,Other}}}).


%%
%% Evaluate constant expressions.
%%

eval_const_expr(Op, {i, Src1}, {i, Src2}, E, Line) ->
    case catch parteval:op(Op, Src1, Src2) of
	Int when integer(Int) ->
	    compile_expr1({integer, Line, Int}, E);
	Float when float(Float) ->
	    compile_expr1({float, Line, Float}, E);
	_ ->
	    false
    end;
eval_const_expr(_, _, _, _, _) ->
    false.

% table_lookup(Key, Table) -> not_found | {found, Value}
% find Value of Key in Table
% Table is a list of elements {Key, Value}
table_lookup(Key, [{Key, Value}|_]) -> 
    {found, Value};
table_lookup(Key, [_|T]) -> 
    table_lookup(Key, T);
table_lookup(Key, []) -> 
    not_found.

% table_insert(Key, Value, Table) -> Table'
% if there is already Key in the Table insert the new Value
table_insert(Key, Value, Table) -> 
    table_insert(Key, Value, Table, []).

table_insert(Key, Value, [{Key, _}|T], NewTable) -> 
    [{Key, Value}|append(T,NewTable)];
table_insert(Key, Value, [H|T], NewTable) -> 
    table_insert(Key, Value, T, [H|NewTable]);
table_insert(Key, Value, [], NewTable) -> 
    [{Key,Value}|NewTable].


% prints the whole table
% used for debuging
print_table([]) -> ok;
print_table([H|T]) ->
    io:put_chars("\t"),
    {Name, Type} = H,
    io:put_chars(Name),
    io:put_chars("\t"),
    io:write(Type),
    io:put_chars("\n"),
    print_table(T).

%%______________________________________________________________________
%% Functions to handle symbol tables


%% fetch/insert Func from/to local function symbol table
l_func_table(Func, Arity) ->
    Table = get(c_l_func),
    FullName = concat([Func, Arity]),
    case table_lookup(FullName, Table) of
	not_found ->
	    I = new_lb(),
	    put(c_l_func, table_insert(FullName, {I, Func, Arity}, Table)),
	    I;
	{found, {I,_,_}} ->
	    I
    end.
						%
mkTestHeap(Words, E) ->
    case where(E) of
	{head_case, _} ->
	    {newTestHeap, Words, 0};
	_ ->
	    {newTestHeap, 0, Words}
    end.

format_error({'EXIT',Reason}) ->
    lists:flatten(io_lib:format("~P\n", [Reason,20]));
format_error(Other) ->
    %% XXX Should be more informative!
    io_lib:format("** Serious error in beam_compile: ~n~p~n"
		  " Please send a copy of the program which caused~n"
		  " this error to the Erlang help desk~n"
		  " support@erlang.ericsson.se~n", [Other]).
