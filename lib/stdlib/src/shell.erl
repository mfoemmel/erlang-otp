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
-module(shell).

-export([start/0, start/1, server/0, server/1, history/1, results/1]).
-export([start_restricted/1, stop_restricted/0]).

-define(LINEMAX, 30).
-define(DEF_HISTORY, 20).
-define(DEF_RESULTS, 20).

start()->
    start(false).

start(NoCtrlG) ->
    code:ensure_loaded(user_default),
    spawn(fun() -> server(NoCtrlG) end).

%% Call this function to start a user restricted shell 
%% from a normal shell session.
start_restricted(RShMod) when atom(RShMod) ->
    case code:ensure_loaded(RShMod) of
	{module,RShMod} -> 
	    ok;
	{error,What} ->
	    io:fwrite("Warning! Restricted shell module ~w not found: ~p~n", 
		      [RShMod,What])
    end,
    application:set_env(stdlib, restricted_shell, RShMod),
    exit('restricted shell starts now').

stop_restricted() ->
    application:unset_env(stdlib, restricted_shell),
    exit('restricted shell stopped').

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

    %% Check if we're in user restricted mode.
    RShErr = 
	case application:get_env(stdlib, restricted_shell) of
	    {ok,RShMod} ->
		io:fwrite("Restricted ", []),
		case code:ensure_loaded(RShMod) of
		    {module,RShMod} -> 
			undefined;
		    {error,What} ->
			{RShMod,What}
		end;
	    undefined ->
		undefined
	end,

    case get(no_control_g) of
	true ->
	    io:fwrite("Eshell V~s~n", [erlang:system_info(version)]);
	_undefined_or_false ->
	    io:fwrite("Eshell V~s  (abort with ^G)~n",
		      [erlang:system_info(version)])
    end,
    erase(no_control_g),

    case RShErr of
	undefined       -> ok;
	{RShMod2,What2} -> io:fwrite("Warning! Restricted shell module ~w not found: ~p~n", 
				     [RShMod2,What2])
    end,

    check_env(shell_history_length, "shell history length"),
    check_env(shell_saved_results, "max number of saved results"),
    History = get_env(shell_history_length, ?DEF_HISTORY),
    Results = get_env(shell_saved_results, ?DEF_RESULTS),
    server_loop(0, start_eval(Bs, []), Bs, [], History, Results).

server_loop(N0, Eval_0, Bs0, Ds0, History0, Results0) ->
    N = N0 + 1,
    {Res, Eval0} = get_command(prompt(N), Eval_0, Bs0, Ds0),
    case Res of 
	{ok,Es0,_EndLine} ->
            case expand_hist(Es0, N) of
                {ok,Es} ->
                    {V,Eval,Bs,Ds} = shell_cmd(Es, Eval0, Bs0, Ds0),
                    History = get_env(shell_history_length, ?DEF_HISTORY),
                    Results = min(History,
                                  get_env(shell_saved_results,?DEF_RESULTS)),
                    add_cmd(N, Es, V),
                    del_cmd(command, N - History, N - History0),
                    del_cmd(result, N - Results, N - Results0),
                    server_loop(N, Eval, Bs, Ds, History, Results);
                {error,E} ->
                    io:fwrite("** ~s **\n", [E]),
                    server_loop(N0, Eval0, Bs0, Ds0, History0, Results0)
            end;
	{error,{Line,Mod,What},_EndLine} ->
	    io:fwrite("** ~w: ~s **\n", [Line,Mod:format_error(What)]),
	    server_loop(N0, Eval0, Bs0, Ds0, History0, Results0);
	{error,terminated} ->			%Io process terminated
	    exit(Eval0, kill),
	    terminated;
	{error,interrupted} ->			%Io process interrupted us
	    exit(Eval0, kill),
	    {_,Eval,_,_} = shell_rep(Eval0, Bs0, Ds0),
	    server_loop(N0, Eval, Bs0, Ds0, History0, Results0);
	{eof,_EndLine} ->
	    io:fwrite("** Terminating erlang (~w) **\n", [node()]),
	    halt();
	eof ->
	    io:fwrite("** Terminating erlang (~w) **\n", [node()]),
	    halt()
    end.

get_command(Prompt, Eval, Bs, Ds) ->
    Parse = fun() -> exit(io:parse_erl_exprs(Prompt)) end,
    Pid = spawn_link(Parse),
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
expand_exprs([], _C) ->
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
expand_expr({'try',L,Es,Scs,Ccs}, C) ->
    {'try',L,expand_exprs(Es, C),expand_cs(Scs, C),expand_cs(Ccs, C)};
expand_expr({'receive',L,Cs}, C) ->
    {'receive',L,expand_cs(Cs, C)};
expand_expr({'receive',L,Cs,To,ToEs}, C) ->
    {'receive',L,expand_cs(Cs, C), expand_expr(To, C), expand_exprs(ToEs, C)};
expand_expr({call,L,{atom,_,e},[N]}, C) ->
    case get_cmd(N, C) of
        {undefined,_} ->
	    no_command(N);
	{[Ce],_V} ->
	    Ce;
	{Ces,_V} when list(Ces) ->
	    {block,L,Ces}
    end;
expand_expr({call,L,{atom,_,v},[N]}, C) ->
    case get_cmd(N, C) of
        {_,undefined} ->
	    no_command(N);
	{Ces,V} when list(Ces) ->
	    {value,L,V}
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
expand_expr(E, _C) ->	 % Constants, including binaries.
    E.

expand_cs([{clause,L,P,G,B}|Cs], C) ->
    [{clause,L,P,G,expand_exprs(B, C)}|expand_cs(Cs, C)];
expand_cs([], _C) ->
    [].

expand_fields([{record_field,L,F,V}|Fs], C) ->
    [{record_field,L,expand_expr(F, C),expand_expr(V, C)}|
     expand_fields(Fs, C)];
expand_fields([], _C) -> [].

expand_quals([{generate,L,P,E}|Qs], C) ->
    [{generate,L,P,expand_expr(E, C)}|expand_quals(Qs, C)];
expand_quals([E|Qs], C) ->
    [expand_expr(E, C)|expand_quals(Qs, C)];
expand_quals([], _C) -> [].

no_command(N) ->
    throw({error,io_lib:fwrite("~s: command not found", [erl_pp:expr(N)])}).

%% add_cmd(Number, Expressions, Value)
%% get_cmd(Number, CurrentCommand)
%% del_cmd(Number)

add_cmd(N, Es, V) ->
    put({command,N}, Es),
    put({result,N}, V).

getc(N) ->
    {get({command,N}), get({result,N})}.

get_cmd(Num, C) ->
    case catch erl_eval:expr(Num, []) of
	{value,N,_} when N < 0 -> getc(C+N);
	{value,N,_} -> getc(N);
	_Other -> {undefined,undefined}
    end.

del_cmd(_Type, N, N0) when N < N0 ->
    ok;
del_cmd(Type, N, N0) ->
    erase({Type,N}),
    del_cmd(Type, N-1, N0).

%% shell_cmd(Sequence, Evaluator, Bindings, Dictionary)
%% shell_rep(Evaluator, Bindings, Dictionary) ->
%%	{Value,Evaluator,Bindings,Dictionary}
%%  Send a command to the evaluator and wait for the reply. Start a new
%%  evaluator if necessary.

shell_cmd(Es, Eval, Bs, Ds) ->
    Eval ! {shell_cmd,self(),{eval,Es}},
    shell_rep(Eval, Bs, Ds).

shell_rep(Ev, Bs0, Ds0) ->
    receive
	{shell_rep,Ev,{value,V,Bs,Ds}} ->
	    io:fwrite("~P~n", [V,?LINEMAX]),
	    {V,Ev,Bs,Ds};
        {shell_rep,Ev,{command_error,{Line,M,Error}}} -> 
	    io:fwrite("** ~w: ~s **\n", [Line,M:format_error(Error)]),
            {{'EXIT',Error},Ev,Bs0,Ds0};
	{shell_req,Ev,get_cmd} ->
	    Ev ! {shell_rep,self(),get()},
	    shell_rep(Ev, Bs0, Ds0);
	{shell_req,Ev,exit} ->
	    Ev ! {shell_rep,self(),exit},
	    io:fwrite("** Terminating shell **\n", []),
	    exit(normal);
	{shell_req,Ev,{update_dict,Ds}} ->	% Update dictionary
	    Ev ! {shell_rep,self(),ok},
	    shell_rep(Ev, Bs0, Ds);
	{'EXIT',Ev,Reason} ->			% It has exited unnaturally
	    io:fwrite("** exited: ~P **\n", [Reason,?LINEMAX]),
	    {{'EXIT',Reason},start_eval(Bs0, Ds0), Bs0, Ds0};
	{'EXIT',_Id,interrupt} ->		% Someone interrupted us
	    exit(Ev, kill),
	    shell_rep(Ev, Bs0, Ds0);
	{'EXIT',_Id,R} ->
	    exit(Ev, R),
	    exit(R);
	_Other ->				% Ignore everything else
	    shell_rep(Ev, Bs0, Ds0)
    end.

start_eval(Bs, Ds) ->
    Self = self(),
    spawn_link(fun() -> evaluator(Self, Bs, Ds) end).

%% evaluator(Shell, Bindings, ProcessDictionary)
%%  Evaluate expressions from the shell. Use the "old" variable bindings
%%  and ductionary.

evaluator(Shell, Bs, Ds) ->
    init_dict(Ds),
    case application:get_env(stdlib, restricted_shell) of
	undefined ->
	    eval_loop(Shell, Bs);
	{ok,RShMod} ->
	    case get(restricted_shell_state) of
		undefined -> put(restricted_shell_state, []);
		_ -> ok
	    end,
	    put(restricted_expr_state, []),
	    restricted_eval_loop(Shell, Bs, RShMod)
    end.

eval_loop(Shell, Bs0) ->
    receive
	{shell_cmd,Shell,{eval,Es}} ->
            {R,Bs2} = 
                case erl_eval:check_command(prep_check(Es), Bs0) of
                    ok ->
                        Lf = {eval,local_func_fun(Shell)},
                        {value,V,Bs} = erl_eval:exprs(Es, Bs0, Lf),
                        {{value,V,Bs,get()},Bs};
                    {error,Error} ->
                        {{command_error,Error},Bs0}
                end,
	    Shell ! {shell_rep,self(),R},
	    eval_loop(Shell, Bs2)
    end.

restricted_eval_loop(Shell, Bs0, RShMod) ->
    receive
	{shell_cmd,Shell,{eval,Es}} ->
            {R,Bs2} = 
                case erl_eval:check_command(prep_check(Es), Bs0) of
                    ok ->
			{LFH,NLFH} = restrict_handlers(RShMod, Shell) ,
			put(restricted_expr_state, []),			
                        {value,V,Bs} = erl_eval:exprs(Es, Bs0, {eval,LFH}, {value,NLFH}),
                        {{value,V,Bs,get()},Bs};
                    {error,Error} ->
                        {{command_error,Error},Bs0}
                end,
	    Shell ! {shell_rep,self(),R},
	    restricted_eval_loop(Shell, Bs2, RShMod)
    end.

restrict_handlers(RShMod, Shell) ->
    { fun(F,As,Binds) -> 
	      local_func_handler(F, As, RShMod, Binds, Shell) 
      end,
      fun(MF,As) -> 
	      non_local_allowed(MF, As, RShMod, Shell) 
      end }.

local_func_handler(F, As, RShMod, Bs, Shell) ->
    case local_allowed(F, As, RShMod, Bs, Shell) of
	disallowed ->
	    {value,{disallowed,{F,As}},Bs};
	{AsEv,Bs1} ->
	    %% The arguments have already been evaluated but local_func/4 
	    %% expects them on abstract form. We can't send the original 
	    %% (unevaluated) arguments since reevaluation may give 
	    %% us unexpected results, so we use erl_parse:abstract/1.
	    AsAbs = lists:map(fun(A) -> erl_parse:abstract(A) end, AsEv),
	    local_func(F, AsAbs, Bs1, Shell)
    end.

local_allowed(F, As, RShMod, Bs, Shell) when atom(F) ->
    {LFH,NLFH} = restrict_handlers(RShMod, Shell) ,
    {AsEv,Bs1} = erl_eval:expr_list(As, Bs, {eval,LFH}, {value,NLFH}),
    case RShMod:local_allowed(F, AsEv, {get(restricted_shell_state),
					get(restricted_expr_state)}) of
	{Result,{RShShSt,RShExprSt}} ->
	    put(restricted_shell_state, RShShSt),
	    put(restricted_expr_state, RShExprSt),
	    if Result == false -> 
		    shell_req(Shell, {update_dict,get()}),
		    exit({disallowed,{F,AsEv}});
	       true -> 
		    {AsEv,Bs1}
	    end
    end.

non_local_allowed(MForFun, As, RShMod, Shell) ->
    case RShMod:non_local_allowed(MForFun, As, {get(restricted_shell_state),
						get(restricted_expr_state)}) of
	{Result,{RShShSt,RShExprSt}} ->
	    put(restricted_shell_state, RShShSt),
	    put(restricted_expr_state, RShExprSt),
	    if Result == false -> 
		    shell_req(Shell, {update_dict,get()}),
		    exit({disallowed,{MForFun,As}});
	       true -> 
		    apply(MForFun, As)
	    end
    end.

prep_check({call,Line,{atom,_,f},[{var,_,_Name}]}) ->
    %% Do not emit a warning for f(V) when V is unbound.
    {atom,Line,ok};
prep_check({value,Line,_Val}) ->
    %% erl_lint cannot handle the history expansion {value,_,_}.
    {atom,Line,ok};
prep_check(T) when tuple(T) ->
    list_to_tuple(prep_check(tuple_to_list(T)));
prep_check([]) ->
    [];
prep_check([E | Es]) ->
    [prep_check(E) | prep_check(Es)];
prep_check(E) ->
    E.

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
local_func(b, [], Bs, _Shell) ->
    {value,list_bindings(erl_eval:bindings(Bs)),Bs};
local_func(f, [], _Bs, _Shell) ->
    {value,ok,erl_eval:new_bindings()};
local_func(f, [{var,_,Name}], Bs, _Shell) ->
    {value,ok,erl_eval:del_binding(Name, Bs)};
local_func(f, [_Other], _Bs, _Shell) ->
    exit({function_clause,[{shell,f,1}]});
local_func(which, [{atom,_,M}], Bs, _Shell) ->
    case erl_eval:binding({module,M}, Bs) of
	{value, M1} ->
	    {value,M1,Bs};
	unbound ->
	    {value,M,Bs}
    end;
local_func(which, [_Other], _Bs, _Shell) ->
    exit({function_clause,[{shell,which,1}]});
local_func(import, [M], Bs, _Shell) ->
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
local_func(import, [_Other], _Bs, _Shell) ->
    exit({function_clause,[{shell,import,1}]});
local_func(import_all, [P], Bs0, _Shell) ->
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
local_func(import_all, [_Other], _Bs, _Shell) ->
    exit({function_clause,[{shell,import_all,1}]});
local_func(use, [M], Bs, Shell) ->
    local_func(import, [M], Bs, Shell);
local_func(use_all, [M], Bs, Shell) ->
    local_func(import_all, [M], Bs, Shell);
local_func(history, [{integer,_,N}], Bs, _Shell) ->
    {value,history(N),Bs};
local_func(history, [_Other], _Bs, _Shell) ->
    exit({function_clause,{shell,history,1}});
local_func(results, [{integer,_,N}], Bs, _Shell) ->
    {value,results(N),Bs};
local_func(results, [_Other], _Bs, _Shell) ->
    exit({function_clause,[{shell,results,1}]});
local_func(exit, [], _Bs, Shell) ->
    shell_req(Shell, exit),			%This terminates us
    exit(normal);
local_func(F, As0, Bs0, Shell) when atom(F) ->
    {As,Bs} = erl_eval:expr_list(As0, Bs0, {eval,local_func_fun(Shell)}),
    case erlang:function_exported(user_default, F, length(As)) of
	true ->
            {eval,{user_default,F},As,Bs};
	false ->
            {eval,{shell_default,F},As,Bs}
    end.

local_func_fun(Shell) ->
    fun(F, As, Bs) -> local_func(F, As, Bs, Shell) end.

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
list_commands([_D|Ds]) ->
    list_commands(Ds);
list_commands([]) -> ok.

list_bindings([{{module,M},Val}|Bs]) ->
    io:fwrite("~p is ~p~n", [M,Val]),
    list_bindings(Bs);
list_bindings([{Name,Val}|Bs]) ->
    case erl_eval:fun_data(Val) of
        {fun_data,_FBs,FCs} ->
            F = {'fun',0,{clauses,FCs}},
            io:fwrite("~s = ~s~n", [Name,erl_pp:expr(F)]);
        false ->
            io:fwrite("~s = ~P~n", [Name,Val,?LINEMAX])
    end,
    list_bindings(Bs);
list_bindings([]) ->
    ok.

min(X, Y) when X < Y ->
    X;
min(_X, Y) ->
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
