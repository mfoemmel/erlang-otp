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
-module(shell).

-export([start/0,start/1,server/0,server/1,evaluator/3,local_func/4,
	 history/1, results/1]).

-define(LINEMAX, 30).
-define(DEF_HISTORY, 20).
-define(DEF_RESULTS, 20).

start()->
    start(false).

start(NoCtrlG) ->
    code:ensure_loaded(user_default),
    spawn(shell, server, [NoCtrlG]).

default_packages() ->
    [].
%%%     ['erl','erl.lang'].

default_modules() ->
    [].
%%%     [{pdict, 'erl.lang.proc.pdict'},
%%%      {keylist, 'erl.lang.list.keylist'},
%%%      {debug, 'erl.system.debug'}].

server(NoCtrlG)->
    put(no_control_g, NoCtrlG),
    server().
    
server() ->
    %% Our spawner has fixed the process groups.
    Bs0 = erl_eval:new_bindings(),
    Bs = lists:foldl(fun ({K, V}, D) ->
			     erl_eval:add_binding({module,K}, V, D)
		     end,
		     lists:foldl(fun (P, D) ->
					 import_all(P, D)
				 end,
				 Bs0, default_packages()),
		     default_modules()),
    %% io:fwrite("Imported modules: ~p.\n", [erl_eval:bindings(Bs)]),
    process_flag(trap_exit, true),
    case get(no_control_g) of
	true ->
	    io:fwrite("Eshell V~s~n", [erlang:system_info(version)]);
	_undefined_or_false ->
	    io:fwrite("Eshell V~s  (abort with ^G)~n",
		      [erlang:system_info(version)])
    end,
    erase(no_control_g),
    check_env(shell_history_length, "shell history length"),
    check_env(shell_saved_results, "max number of saved results"),
    History = get_env(shell_history_length, ?DEF_HISTORY),
    Results = get_env(shell_saved_results, ?DEF_RESULTS),
    server_loop(0, start_eval(Bs, []), Bs, [], History, Results).

server_loop(N0, Eval_0, Bs0, Ds0, History0, Results0) ->
    N = N0 + 1,
    {Res, Eval0} = get_command(prompt(N), Eval_0, Bs0, Ds0),
    case Res of 
	{ok,Es0,EndLine} ->			%Commands
	    case expand_hist(Es0, N) of
		{ok,Es} ->
		    {V,Eval,Bs,Ds} = shell_cmd(Es, Eval0, Bs0, Ds0, N),
		    History = get_env(shell_history_length, ?DEF_HISTORY),
		    Results = min(History,
				  get_env(shell_saved_results, ?DEF_RESULTS)),
		    add_cmd(N, Es, V),
		    del_cmd(command, N - History, N - History0),
		    del_cmd(result, N - Results, N - Results0),
		    server_loop(N, Eval, Bs, Ds, History, Results);
		{error,E} ->
		    io:fwrite("** ~s **\n", [E]),
		    server_loop(N0, Eval0, Bs0, Ds0, History0, Results0)
	    end;
	{error,{Line,Mod,What},EndLine} ->
	    io:fwrite("** ~w: ~s **\n", [Line,apply(Mod,format_error,[What])]),
	    server_loop(N0, Eval0, Bs0, Ds0, History0, Results0);
	{error,terminated} ->			%Io process terminated
	    exit(Eval0, kill),
	    terminated;
	{error,interrupted} ->			%Io process interrupted us
	    exit(Eval0, kill),
	    {_,Eval,_,_} = shell_rep(Eval0, Bs0, Ds0),
	    server_loop(N0, Eval, Bs0, Ds0, History0, Results0);
	{eof,EndLine} ->
	    io:fwrite("** Terminating erlang (~w) **\n", [node()]),
	    halt();
	eof ->
	    io:fwrite("** Terminating erlang (~w) **\n", [node()]),
	    halt()
    end.

get_command(Prompt, Eval, Bs, Ds) ->
    Parse = fun() -> exit(io:parse_erl_exprs(Prompt)) end,
    Pid = spawn_link(erlang, apply, [Parse, []]),
    get_command1(Pid, Eval, Bs, Ds).

get_command1(Pid, Eval, Bs, Ds) ->
    receive
	{'EXIT', Pid, Res} ->
	    {Res, Eval};
	{'EXIT', Eval, Reason} ->
	    io:fwrite("** exited: ~P **\n", [Reason, ?LINEMAX]),
	    get_command1(Pid, start_eval(Bs, Ds), Bs, Ds)
    end.

prompt(N) ->
    case is_alive() of
	true -> {format,"(~s)~w> ",[node(),N]};
	false -> {format,"~w> ",[N]}
    end.

%% expand_hist(Expressions, CommandNumber)
%%  Preprocess the expression list replacing all history list commands
%%  with their expansions.

expand_hist(Es, C) ->
    catch {ok,expand_exprs(Es, C)}.

expand_exprs([E|Es], C) ->
    [expand_expr(E, C)|expand_exprs(Es, C)];
expand_exprs([], C) ->
    [].

expand_expr({cons,L,H,T}, C) ->
    {cons,L,expand_expr(H, C),expand_expr(T, C)};
expand_expr({lc,L,E,Qs}, C) ->
    {lc,L,expand_expr(E, C),expand_quals(Qs, C)};
expand_expr({tuple,L,Elts}, C) ->
    {tuple,L,expand_exprs(Elts, C)};
expand_expr({record_index,L,Name,F}, C) ->
    {record_index,L,Name,expand_expr(F, C)};
expand_expr({record,L,Name,Is}, C) ->
    {record,L,Name,expand_fields(Is, C)};
expand_expr({record_field,L,R,Name,F}, C) ->
    {record_field,L,expand_expr(R, C),Name,expand_expr(F, C)};
expand_expr({record,L,R,Name,Ups}, C) ->
    {record,L,expand_expr(R, C),Name,expand_fields(Ups, C)};
expand_expr({record_field,L,R,F}, C) ->		%This is really illegal!
    {record_field,L,expand_expr(R, C),expand_expr(F, C)};
expand_expr({block,L,Es}, C) ->
    {block,L,expand_exprs(Es, C)};
expand_expr({'if',L,Cs}, C) ->
    {'if',L,expand_cs(Cs, C)};
expand_expr({'case',L,E,Cs}, C) ->
    {'case',L,expand_expr(E, C),expand_cs(Cs, C)};
expand_expr({'receive',L,Cs}, C) ->
    {'receive',L,expand_cs(Cs, C)};
expand_expr({'receive',L,Cs,To,ToEs}, C) ->
    {'receive',L,expand_cs(Cs, C), expand_expr(To, C), expand_exprs(ToEs, C)};
expand_expr({call,L,{atom,_,e},[N]}, C) ->
    case get_cmd(N, C) of
	{[Ce],V} ->
	    Ce;
	{Ces,V} ->
	    {block,L,Ces};
	undefined ->
	    no_command(N)
    end;
expand_expr({call,L,{atom,_,v},[N]}, C) ->
    case get_cmd(N, C) of
	{Ces,V} ->
	    {value,L,V};
	undefined ->
	    no_command(N)
    end;
expand_expr({call,L,F,Args}, C) ->
    {call,L,expand_expr(F, C),expand_exprs(Args, C)};
expand_expr({'catch',L,E}, C) ->
    {'catch',L,expand_expr(E, C)};
expand_expr({match,L,Lhs,Rhs}, C) ->
    {match,L,Lhs,expand_expr(Rhs, C)};
expand_expr({op,L,Op,Arg}, C) ->
    {op,L,Op,expand_expr(Arg, C)};
expand_expr({op,L,Op,Larg,Rarg}, C) ->
    {op,L,Op,expand_expr(Larg, C),expand_expr(Rarg, C)};
expand_expr({remote,L,M,F}, C) ->
    {remote,L,expand_expr(M, C),expand_expr(F, C)};
expand_expr(E, C) ->				%All constants
    E.

expand_cs([{clause,L,P,G,B}|Cs], C) ->
    [{clause,L,P,G,expand_exprs(B, C)}|expand_cs(Cs, C)];
expand_cs([], C) ->
    [].

expand_fields([{record_field,L,F,V}|Fs], C) ->
    [{record_field,L,expand_expr(F, C),expand_expr(V, C)}|
     expand_fields(Fs, C)];
expand_fields([], C) -> [].

expand_quals([{generate,L,P,E}|Qs], C) ->
    [{generate,L,P,expand_expr(E, C)}|expand_quals(Qs, C)];
expand_quals([E|Qs], C) ->
    [expand_expr(E, C)|expand_quals(Qs, C)];
expand_quals([], C) -> [].

no_command(N) ->
    throw({error,io_lib:fwrite("~s: command not found", [erl_pp:expr(N)])}).

%% add_cmd(Number, Expressions, Value)
%% get_cmd(Number, CurrentCommand)
%% del_cmd(Number)

add_cmd(N, Es, V) ->
    put({command,N}, Es),
    put({result,N}, V).

getc(N) ->
    {get({command,N}),
     get({result,N})}.

get_cmd(Num, C) ->
    case catch erl_eval:expr(Num, []) of
	{value,N,_} when N < 0 -> getc(C+N);
	{value,N,_} -> getc(N);
	Other -> undefined
    end.

del_cmd(Type, N, N0) when N < N0 ->
    ok;
del_cmd(Type, N, N0) ->
    erase({Type,N}),
    del_cmd(Type, N-1, N0).

%% shell_cmd(Sequence, Evaluator, Bindings, Dictionary, CommandNumber)
%% shell_rep(Evaluator) ->
%%	{Value,Evaluator,Bindings,Dictionary}
%%  Send a command to the evaluator and wait for the reply. Start a new
%%  evaluator if necessary.

shell_cmd(Es, Eval, Bs, Ds, N) ->
    Eval ! {shell_cmd,self(),{eval,Es}},
    shell_rep(Eval, Bs, Ds).

shell_rep(Ev, Bs0, Ds0) ->
    receive
	{shell_rep,Ev,{value,V,Bs,Ds}} ->
	    io:fwrite("~P~n", [V,?LINEMAX]),
	    {V,Ev,Bs,Ds};
	{shell_req,Ev,get_cmd} ->
	    Ev ! {shell_rep,self(),get()},
	    shell_rep(Ev, Bs0, Ds0);
	{shell_req,Ev,exit} ->
	    Ev ! {shell_rep,self(),exit},
	    io:fwrite("** Terminating shell **\n", []),
	    exit(normal);
	{'EXIT',Ev,Reason} ->			%It has exited unnaturally
	    io:fwrite("** exited: ~P **\n", [Reason,?LINEMAX]),
	    {{'EXIT',Reason},start_eval(Bs0, Ds0), Bs0, Ds0};
	{'EXIT',Id,interrupt} ->		%Someone interrupted us
	    exit(Ev, kill),
	    shell_rep(Ev, Bs0, Ds0);
	{'EXIT',Id,R} ->
	    exit(Ev, R),
	    exit(R);
	Other ->				%Ignore everything else
	    shell_rep(Ev, Bs0, Ds0)
    end.

start_eval(Bs, Ds) ->
    spawn_link(shell, evaluator, [self(),Bs,Ds]).

%% evaluator(Shell, Bindings, ProcessDictionary)
%%  Evaluate expressions from the shell. Use the "old" variable bindings
%%  and ductionary.

evaluator(Shell, Bs, Ds) ->
    init_dict(Ds),
    eval_loop(Shell, Bs).

eval_loop(Shell, Bs0) ->
    receive
	{shell_cmd,Shell,{eval,Es}} ->
	    {value,V,Bs} = erl_eval:exprs(Es, Bs0,
					  {eval,{shell,local_func},[Shell]}),
	    Shell ! {shell_rep,self(),{value,V,Bs,get()}},
	    eval_loop(Shell, Bs)
    end.

init_dict([{K,V}|Ds]) ->
    put(K, V),
    init_dict(Ds);
init_dict([]) -> true.

%% local_func(Function, Args, Bindings, Shell) ->
%%	{value,Val,Bs}
%%  Evaluate local functions, including shell commands.

local_func(h, [], Bs, Shell) ->
    Cs = shell_req(Shell, get_cmd),
    Cs1 = lists:filter(fun({{command, _},_}) -> true;
			  ({{result, _},_}) -> true;
			  (_) -> false
		       end,
		       Cs),
    Cs2 = lists:map(fun({{T, N}, V}) -> {{N, T}, V} end,
		    Cs1),
    Cs3 = lists:keysort(1, Cs2),
    {value,list_commands(Cs3),Bs};
local_func(b, [], Bs, Shell) ->
    {value,list_bindings(erl_eval:bindings(Bs)),Bs};
local_func(f, [], Bs, Shell) ->
    {value,ok,[]};
local_func(f, [{var,_,Name}], Bs, Shell) ->
    {value,ok,erl_eval:del_binding(Name, Bs)};
local_func(f, [Other], Bs, Shell) ->
    exit({function_clause,{shell,f,1}});
local_func(which, [{atom,_,M}], Bs, Shell) ->
    case erl_eval:binding({module,M}, Bs) of
	{value, M1} ->
	    {value,M1,Bs};
	unbound ->
	    {value,M,Bs}
    end;
local_func(which, [Other], Bs, Shell) ->
    exit({function_clause,{shell,which,1}});
local_func(import, [M], Bs, Shell) ->
    case erl_parse:package_segments(M) of
	error -> exit({badarg,[{shell,import,1}]});
	M1 ->
	    Mod = package_to_string(M1),
	    case packages:is_valid(Mod) of
		true ->
		    Key = list_to_atom(packages:last(Mod)),
		    Mod1 = list_to_atom(Mod),
		    {value,ok,erl_eval:add_binding({module,Key}, Mod1, Bs)};
		false ->
		    exit({{bad_module_name, Mod}, [{shell,import,1}]})
	    end
    end;
local_func(import, [Other], Bs, Shell) ->
    exit({function_clause,[{shell,import,1}]});
local_func(import_all, [P], Bs0, Shell) ->
    case erl_parse:package_segments(P) of
	error -> exit({badarg,[{shell,import_all,1}]});
	P1 ->
	    Name = package_to_string(P1),
	    case packages:is_valid(Name) of
		true ->
		    Bs1 = import_all(Name, Bs0),
		    {value,ok,Bs1};
		false ->
		    exit({{bad_package_name, Name},
			  [{shell,import_all,1}]})
	    end
    end;
local_func(import_all, [Other], Bs, Shell) ->
    exit({function_clause,[{shell,import_all,1}]});
local_func(use, [M], Bs, Shell) ->
    local_func(import, [M], Bs, Shell);
local_func(use_all, [M], Bs, Shell) ->
    local_func(import_all, [M], Bs, Shell);
local_func(history, [{integer,_,N}], Bs, Shell) ->
    {value,history(N),Bs};
local_func(history, [Other], Bs, Shell) ->
    exit({function_clause,{shell,history,1}});
local_func(results, [{integer,_,N}], Bs, Shell) ->
    {value,results(N),Bs};
local_func(results, [Other], Bs, Shell) ->
    exit({function_clause,{shell,results,1}});
local_func(exit, [], Bs, Shell) ->
    shell_req(Shell, exit),			%This terminates us
    exit(normal);
local_func(F, As0, Bs0, Shell) when atom(F) ->
    {As,Bs} = erl_eval:expr_list(As0, Bs0, {eval,{shell,local_func},[Shell]}),
    case erlang:function_exported(user_default, F, length(As)) of
	true ->
	    {value,apply(user_default, F, As),Bs};
	false ->
	    {value,apply(shell_default, F, As),Bs}
    end;
local_func(F, As0, Bs0, Shell) ->
    {As,Bs} = erl_eval:expr_list(As0, Bs0, {eval,{shell,local_func},[Shell]}),
    {value,apply(F, As),Bs}.

import_all(P, Bs0) ->
    Ms = packages:find_modules(P),
    lists:foldl(fun (M, Bs) ->
			Key = list_to_atom(M),
			M1 = list_to_atom(packages:concat(P, M)),
			erl_eval:add_binding({module,Key}, M1, Bs)
		end,
		Bs0, Ms).

shell_req(Shell, Req) ->
    Shell ! {shell_req,self(),Req},
    receive
	{shell_rep,Shell,Rep} -> Rep
    end.

list_commands([{{N,command},Es}, {{N,result}, V} |Ds]) ->
    io:requests([{format,"~w: ~s~n",[N,erl_pp:exprs(Es)]},
		 {format,"-> ~P~n",[V,?LINEMAX]}]),
    list_commands(Ds);
list_commands([{{N,command},Es} |Ds]) ->
    io:requests([{format,"~w: ~s~n",[N,erl_pp:exprs(Es)]}]),
    list_commands(Ds);
list_commands([D|Ds]) ->
    list_commands(Ds);
list_commands([]) -> ok.

list_bindings([{Name,Val}|Bs]) ->
    io:fwrite("~s = ~P~n", [Name,Val,?LINEMAX]),
    list_bindings(Bs);
list_bindings([]) ->
    ok.

min(X, Y) when X < Y ->
    X;
min(X, Y) ->
    Y.

get_env(V, Def) ->
    case application:get_env(stdlib, V) of
	{ok, Val} when integer(Val) ->
	    Val;
	_ ->
	    Def
    end.
	    
check_env(V, Name) ->
    case application:get_env(stdlib, V) of
	undefined ->
	    ok;
	{ok, Val} when integer(Val) ->
	    ok;
	{ok, Val} ->
	    Txt = io_lib:format("Invalid ~s ~p~n", [Name, Val]),
	    error_logger:info_report(lists:flatten(Txt)),
	    ok
    end.
	    
set_env(App, Name, Val, Default) ->
    Prev = case application:get_env(App, Name) of
	       undefined ->
		   Default;
	       {ok, Old} ->
		   Old
    end,
    application_controller:set_env(App, Name, Val),
    Prev.

history(L) when integer(L), L >= 0 ->
    set_env(stdlib, shell_history_length, L, ?DEF_HISTORY).

results(L) when integer(L), L >= 0 ->
    set_env(stdlib, shell_saved_results, L, ?DEF_RESULTS).

%% In syntax trees, module/package names are atoms or lists of atoms.

package_to_string(A) when atom(A) -> atom_to_list(A);
package_to_string(L) when list(L) -> packages:concat(L).
