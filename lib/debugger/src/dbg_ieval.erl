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
%% Purpose : An evaluator for Erlang abstract syntax.

-module(dbg_ieval).

-export([eval_function/4,eval_expr/6]).

-export([format_args/1,
	 trace_out/1,
	 exit/4,
	 exit/5,
	 pop/1,
	 get_stack/0,
	 in_use_p/2,
	 trace_fnk_ret/3
	]).

-import(dbg_imeta, [add_catch_lev/0,dec_catch_lev/0,get_catch_lev/0,
		    main_meta_loop/7]).

-import(lists, [reverse/1,reverse/2]).

%% FIXME: My understanding of the contents of a stack_frame
%% FIXME: Needs to be implemented throughout all modules.
-define(stack_frame,{level,
		     module,			%Current module
		     function,			%Current function, name/arity
		     line,			%Current line (source code?)
		     bindings			%Variable bindings
		     }).


eval_function(Mod, Func, As, Le) ->
    catch eval_function(Mod, Func, As, [], Mod, false, Le, extern, -1, local).

%% seq(ExpressionSeq, Bindings, CurrentModule, LastCall, Level, Function)
%%  Returns:
%%	{value,Value,NewBindings}

seq([E], Bs0, Cm, Lc, Le, F) ->
    case dbg_icmd:cmd(E,Bs0,Cm,Le,F) of
	{skip,Bs} ->
	    {value,skipped,Bs};
	Bs ->
	    expr(E, Bs, Cm, Lc, Le, F)
    end;
seq([E|Es], Bs0, Cm, Lc, Le, F) ->
    case dbg_icmd:cmd(E,Bs0,Cm,Le,F) of
	{skip,Bs} ->
	    seq(Es, Bs, Cm, Lc, Le, F);
	Bs1 ->
	    {value,_,Bs} = expr(E, Bs1, Cm, false, Le, F),
	    seq(Es, Bs, Cm, Lc, Le, F)
    end;
seq([], Bs, Cm, _, _, _) ->
    {value,true,Bs}.

%% eval_expr(Expression, Bindings, CurrentModule, LastCall, Level, Function)
%%  Evalutes a shell expression in the real process.
%%
%%  Returns:
%%	{value,Value,NewBindings}

eval_expr(E, Bs, Cm, Lc, Le, F) ->
    Msg_handler = get(self),
    Msg_handler ! {sys,self(),{eval,E,Bs}},
    Line = element(2, E),
    main_meta_loop(Msg_handler, Bs, Le, Lc, Cm, Line, F).

%% expr(Expression, Bindings, CurrentModule, LastCall, Level, Function)
%%  Returns:
%%	{value,Value,NewBindings}

%% Variable
%%
expr({var,Line,V}, Bs, Cm, _, _, F) ->
    case binding(V, Bs) of
	{value,Val} ->
	    {value,Val,Bs};
	unbound ->
	    exit({{unbound,V},F},Cm,Line,Bs)
    end;

expr({value,Line,Val}, Bs, _, _, _, _) ->
    {value,Val,Bs};
expr({value,Val}, Bs, _, _, _, _) ->		%Special case straight values.
    {value,Val,Bs};

%% A cons (list)
%%
expr({cons,Line,H0,T0}, Bs0, Cm, _, Le, F) ->
    {value,H,Bs1} = expr(H0, Bs0, Cm, false, Le, F),
    {value,T,Bs2} = expr(T0, Bs0, Cm, false, Le, F),
    {value,[H|T],merge_bindings(Bs2, Bs1, Cm, Line, F)};

%% Tuple
%%
expr({tuple,Line,Es0}, Bs0, Cm, _, Le, F) ->
    {Vs,Bs} = eval_list(Es0, Bs0, Cm, Le, Line, F),
    {value,list_to_tuple(Vs),Bs};

%% A block of statements
%%
expr({block,_,Es}, Bs, Cm, Lc, Le, F) ->
    seq(Es, Bs, Cm, Lc, Le, F);

%% Catch statement
%%
expr({'catch',_,Expr}, Bs0, Cm, _, Le, F) ->
    add_catch_lev(),
    Msg_handler = get(self),
    Ref = make_ref(),
    case catch {Ref,expr(Expr, Bs0, Cm, false, Le, F)} of
	{Ref,Val} ->
	    dec_catch_lev(),
	    pop(Le),
	    Val;
	{'EXIT',{Msg_handler,Reason}} ->
	    pop(Le),
	    exit({Msg_handler,Reason});
	Other ->
	    dec_catch_lev(),
	    pop(Le),
	    {value,Other,Bs0}
    end;

%% Case statement
%%
expr({'case',Line,E,Cs}, Bs0, Cm, Lc, Le, F) ->
    {value,Val,Bs} = expr(E, Bs0, Cm, false, Le, F),
    case_clauses(Val, Cs, Bs, Cm, Lc, Le, Line, F);

%% If statement
%%
expr({'if',Line,Cs}, Bs, Cm, Lc, Le, F) ->
    if_clauses(Cs, Bs, Cm, Lc, Le, Line, F);

%% Matching expression (=)
%%
expr({match,Line,Lhs,Rhs0}, Bs0, Cm, Lc, Le, F) ->
    {value,Rhs,Bs1} = expr(Rhs0, Bs0, Cm, false, Le, F),
    case match(Lhs, Rhs, Bs1) of
	{match,Bs} ->
	    {value,Rhs,Bs};
	nomatch ->
	    exit({{badmatch,Rhs},F},Cm,Line,Bs1)
    end;

%%
%% Construct a fun.
%%
expr({make_fun,Line,Index,Uniq,OldIndex,OldUniq,Free0}, Bs, Cm, Lc, Le, F) ->
    Free = get_free_vars(Free0, Bs),
    {value,erts_debug:make_fun({get(self),Cm,Index,Uniq,OldIndex,OldUniq,Free}),Bs};

%%
%% Local function call
%%
expr({local_call,Line,Func,As0}, Bs0, Cm, Lc, Le, F) ->
    {As,Bs} = eval_list(As0, Bs0, Cm, Le, Line, F),
    trace_fnk_call(Le, Line, Cm, Func, As),
    eval_function(Cm, Func, As, Bs, Cm, Lc, Le, F, Line, local);

%%
%% Remote function call.
%%
expr({call_remote,Line,Mod,Func,As0}, Bs0, Cm, Lc, Le, F) ->
    {As,Bs} = eval_list(As0, Bs0, Cm, Le, Line, F),
    trace_fnk_call(Le, Line, Mod, Func, As),
    eval_function(Mod, Func, As, Bs, Cm, Lc, Le, F, Line, extern);

%%
%% Call to self/0 (optimization).
%%
expr(self, Bs, Cm, Lc, Le, F) ->
    {value,get(self),Bs};

%%
%% Call to "safe" BIF (a BIF that can safely be executed in the meta process).
%%
expr({safe_bif,Line,Mod,Func,As0}, Bs0, Cm, Lc, Le, F) ->
    {As,Bs} = eval_list(As0, Bs0, Cm, Le, Line, F),
    safe_bif(catch_bif(get_catch_lev()), Func, As, Bs, Cm, Lc, Le, Line, F);

%%
%% Call to a BIF that must be evaluated in the correct process.
%%
expr({bif,Line,Mod,Func,As0}, Bs0, Cm, Lc, Le, F) ->
    {As,Bs} = eval_list(As0, Bs0, Cm, Le, Line, F),
    Msg_handler = get(self),
    trace_bif_call(Le, Line, Func, As),
    Msg_handler ! {sys,self(),{catch_bif(get_catch_lev()),Mod,Func,As,
			       clean_mfa(F),false}},
    main_meta_loop(Msg_handler, Bs, Le, Lc, Cm, Line, F);

%%
%% Call to a BIF that spawns a new process.
%%
expr({spawn_bif,Line,Mod,Func,As0}, Bs0, Cm, Lc, Le, F) ->
    {As,Bs} = eval_list(As0, Bs0, Cm, Le, Line, F),
    Msg_handler = get(self),
    trace_bif_call(Le, Line, Func, As),
    Msg_handler ! {sys,self(),{catch_bif(get_catch_lev()),Mod,Func,As,
			       clean_mfa(F),follow_mod()}},
    main_meta_loop(Msg_handler, Bs, Le, Lc, Cm, Line, F);

%%
%% Call to an operation.
%%
expr({op,Line,Op,As0}, Bs0, Cm, Lc, Le, F) ->
    {As,Bs} = eval_list(As0, Bs0, Cm, Le, Line, F),
    case catch apply(erlang, Op, As) of
	{'EXIT',{Reason,Where}} ->
	    exit({Reason,F}, Cm, Line, Bs);
	Value ->
	    {value,Value,Bs}
    end;

%%
%% apply/2 (fun)
%%
expr({apply_fun,Line,Fun0,As0}, Bs0, Cm, Lc, Le, F) ->
    case expr(Fun0, Bs0, Cm, Lc, Le, F) of
	{value,Fun,Bs1} when function(Fun) ->
	    {As,Bs} = eval_list(As0, Bs1, Cm, Le, Line, F),
	    eval_function(unknown, Fun, As, Bs, Cm, Lc, Le, F, Line, extern);
	{value,{Mod,Func},Bs1} when atom(Mod), atom(Func) ->
	    {As,Bs} = eval_list(As0, Bs1, Cm, Le, Line, F),
	    eval_function(Mod, Func, As, Bs, Cm, Lc, Le, F, Line, extern);
	{value,BadFun,Bs1} ->
	    exit({{badfun,BadFun},F}, Cm, Line, Bs1)
    end;

%%
%% apply/3
%%
expr({apply,Line,As0}, Bs0, Cm, Lc, Le, F) ->
    {[Mod,Name,As],Bs} = eval_list(As0, Bs0, Cm, Le, Line, F),
    eval_function(Mod, Name, As, Bs, Cm, Lc, Le, F, Line, extern);
    
%%
%% Throw.
%%
expr({throw,Line,As0}, Bs0, Cm, Lc, Le, F) ->
    {value,Term,Bs} = expr(As0, Bs0, Cm, Lc, Le, F),
    Msg_handler = get(self),
    trace_bif_call(Le, Line, throw, [Term]),
    case get_catch_lev() of
	0 ->					% No surrounding catch interpreted
	    Msg_handler ! {sys,self(),
			   {catch_bif(get_catch_lev()),erlang,throw,[Term],
			    clean_mfa(F),false}},
	    main_meta_loop(Msg_handler, Bs, Le, Lc, Cm, Line, F);
	N ->
	    throw(Term)
    end;

%%
%% Mod:module_info/0,1
%%
expr({module_info_0,Line,Mod}, Bs, Cm, Lc, Le, F) ->
    {value,[{compile,module_info(Mod, compile)},
	    {attributes,module_info(Mod, attributes)},
	    {imports,module_info(Mod, imports)},
	    {exports,module_info(Mod, exports)}],Bs};
expr({module_info_1,Line,Mod,[As0]}, Bs0, Cm, Lc, Le, F) ->
    {value,What,Bs} = expr(As0, Bs0, Cm, Lc, Le, F),
    {value,module_info(Mod, What),Bs};

%%
%% Receive statement
%%
expr({'receive',Line,Cs}, Bs0, Cm, Lc, Le, F) ->
    trace_receive(Le,false),
    eval_receive(get(self),Cs,Bs0,Cm,Lc,Le,Line,F);


%% Receive statement. FIXME: What's the difference
%%
expr({'receive',Line,Cs,To,ToExprs}, Bs0, Cm, Lc, Le, F) ->
    {value,ToVal,ToBs} = expr(To,Bs0,Cm,false,Le,F),
    trace_receive(Le,true),
    check_timeoutvalue(ToVal, ToBs, To, Cm, F),
    {Stamp,_} = statistics(wall_clock),
    eval_receive(get(self),Cs,ToVal,ToExprs,ToBs,Bs0,Cm,Lc,Le,Line,0,Stamp,F);

%% Send (!)
%%
expr({send,Line,T0,M0}, Bs0, Cm, Lc, Le, F) ->
    {value,T,Bs1} = expr(T0, Bs0, Cm, false, Le, F),
    {value,M,Bs2} = expr(M0, Bs0, Cm, false, Le, F),
    Bs = merge_bindings(Bs2, Bs1, Cm, Line, F),
    eval_send(Line, T, M, Bs, Cm, F);

expr({bin,_,Fs}, Bs0, Cm, Lc, Le, F) ->
    eval_bits:expr_grp(Fs,Bs0,
		       fun(E,B) -> expr(E,B,Cm,Lc,Le,F) end,
		       [],
		       false);

expr({lc,Line,E,Qs}, Bs, Cm, Lc, Le, F) ->
    eval_lc(E, Qs, Bs, Cm, Lc, Le, F);

%% Brutal exit on unknown expressions/clauses/values/etc.
%%
expr(E, _, _, _, _, _) ->			%Not Yet Implemented
    exit({'NYI',E}).

%% eval_lc(Expr, [Qualifier], Bindings, Mod, LastCall, Level, Func) ->
%%	{value,Value,Bindings}.
%%  This is evaluating list comprehensions "straight out of the book".
%%  Copied from rv's implementation in erl_eval.

eval_lc(E, Qs, Bs, Cm, Lc, Le, F) ->
    {value,eval_lc1(E, Qs, Bs, Cm, Lc, Le, F), Bs}.

eval_lc1(E, [{generate,_,P,L0}|Qs], Bs0, Cm, Lc, Le, F) ->
    {value,L1,Bs1} = expr(L0, Bs0, Cm, false, Le, F),
    lists:flatmap(fun (V) ->
		    case match(P, V, []) of
			{match,Bsn} ->
			    Bs2 = add_bindings(Bsn, Bs1),
			    eval_lc1(E, Qs, Bs2, Cm, Lc, Le, F);
			nomatch -> []
		    end end, L1);
eval_lc1(E, [{guard,Q}|Qs], Bs0, Cm, Lc, Le, F) ->
    case guard(Q, Bs0) of
	true -> eval_lc1(E, Qs, Bs0, Cm, Lc, Le, F);
	false -> []
    end;
eval_lc1(E, [Q|Qs], Bs0, Cm, Lc, Le, F) ->
    case expr(Q, Bs0, Cm, false, Le, F) of
	{value,true,Bs1} -> eval_lc1(E, Qs, Bs1, Cm, Lc, Le, F);
	{value,false,Bs1} -> [];
	Other -> exit({bad_filter,in_fnk(F)})
    end;
eval_lc1(E, [], Bs, Cm, Lc, Le, F) ->
    {value,V,_} = expr(E, Bs, Cm, false, Le, F),
    [V].

module_info(Mod, module) -> Mod;
module_info(Mod, compile) -> [];
module_info(Mod, attributes) ->
    {ok,Attr} = dbg_idb:lookup(Mod, attributes),
    Attr;
module_info(Mod, imports) -> [];
module_info(Mod, exports) ->
    {ok,Attr} = dbg_idb:lookup(Mod, exports),
    Attr;
module_info(Mod, functions) -> [].

get_free_vars([V|Vs], Bs) ->
    {value,Val} = binding(V, Bs),
    [Val|get_free_vars(Vs, Bs)];
get_free_vars([], Bs) -> [].

%% exit/4 -
%% Store the error reason and position if not
%% stored already.
%% Terminate according to type of exit.
%% -------------------------------------------------

exit({What,Where}, Cm, Line, Bs) when tuple(Where) ->
    Reason = {What,in_fnk(Where)},
    put_error(Reason, Cm, Line, Bs),
    exit(Reason);
exit(Reason, Cm, Line, Bs) ->
    put_error(Reason, Cm, Line, Bs),
    exit(Reason).

%% exit/5

exit(Msg_handler,Reason,Cm,Line,Bs) ->  % We will only die if the real
    put_error(Reason,Cm,Line,Bs),       % process has terminated.
    exit({Msg_handler,Reason}).
    
put_error(Reason,Cm,Line,Bs) ->
    case get(error) of
	none when Line =/= -1 ->
	    do_put_error(Reason,Cm,Line,Bs);
	{R,_,_,_} when R =/= Reason, Line =/= -1 ->
	    do_put_error(Reason,Cm,Line,Bs);
	{Reason,{C,_},_,_} when C =/= Cm, Line =/= -1 ->
	    do_put_error(Reason,Cm,Line,Bs);
	{Reason,{Cm,L},_,_} when L =/= Line, Line =/= -1 ->
	    do_put_error(Reason,Cm,Line,Bs);
	_ ->
	    ok
    end.

do_put_error(Reason, Cm, Line, Bs) ->
    BinStack = term_to_binary(get_stack()),
    put(error, {Reason,{Cm,Line},Bs,BinStack}),
    ok.

%% -------------------------------------------------

eval_function(_Mod, Fun, As0, Bs, Cm, Lc, Le, F, Line, _Called) when function(Fun) ->
    push(Bs, F, Cm, Lc, Le, Line),
    case dbg_imeta:lambda(Fun, As0) of
	undef ->
	    exit({undef,[{Fun,As0}|in_fnk(F)]}, Cm, Line, Bs);
	badfun ->
	    exit({{badfun,Fun},F}, Cm, Line, Bs);
	badarity ->
	    exit({badarity,[{Fun,As0}|in_fnk(F)]}, Cm, Line, Bs);
	not_interpreted ->
	    Msg_handler = get(self),
	    Msg_handler ! {sys,self(),{catch_apply(get_catch_lev()),
				       erlang,apply,[Fun,As0]}},
	    %% Enter meta-wait state
	    {value,Val,_} = main_meta_loop(Msg_handler,Bs,Le+1,false,Cm,Line,F),
	    pop(Le),
	    {value,Val,Bs};
	{Cs,Mod,Name,As} when Lc == false ->
	    {value,Val,_} = fnk_clauses(Name,Cs,As,Mod,true,Le+1,Cm,Line,Bs),
	    trace_fnk_ret(Le,Val,Lc),
	    pop(Le),
	    {value,Val,Bs};
	{Cs,Mod,Name,As} ->
	    %% Note, this MUST be the last call, since we are interpreting
	    %% code that executes their last call.
	    fnk_clauses(Name,Cs,As,Mod,Lc,Le+1,Cm,Line,Bs)
    end;
eval_function(Mod, Name, As, Bs, Cm, Lc, Le, F, Line, Called) ->
    push(Bs,F,Cm,Lc,Le,Line),
    case dbg_imeta:function(Mod, Name, As, Called) of

	%% Continue evaluation in meta process.
	Cs when list(Cs), Lc == false ->
	    {value,Val,_} = fnk_clauses(Name,Cs,As,Mod,true,Le+1,Cm,Line,Bs),
	    trace_fnk_ret(Le,Val,Lc),
	    pop(Le),
	    {value,Val,Bs};
	Cs when list(Cs) ->
	    %% Note, this MUST be the last call, since we are interpreting
	    %% code that executes their last call.
	    fnk_clauses(Name,Cs,As,Mod,Lc,Le+1,Cm,Line,Bs);

	%% Continue evaluation in non-interpreted code.
	not_interpreted ->
	    Msg_handler = get(self),
	    Msg_handler ! {sys,self(),{catch_apply(get_catch_lev()),Mod,Name,As}},
	    {value,Val,_} = main_meta_loop(Msg_handler,Bs,Le+1,false,Cm,Line,F),
	    pop(Le),
	    {value,Val,Bs};
	undef ->
	    exit({undef,[{Mod,Name,As}|in_fnk(F)]}, Cm, Line, Bs)
    end.

catch_apply(0) -> apply;
catch_apply(Lev) when integer(Lev) -> catch_apply.


%% ---
%% Hold a stack which can be inspected, at break and exit.
%%  push(Bs,CurFnk,Cm,Lc,Le,Line),
%%
%% The push-function is conditional.
%% It determines whether or not to push stuff on the stack
%% by looking at the stack_trace option stored in the process 
%% dictonary
%% 
push(Bs, F, Cm, Lc, Le, Line) when Line > 0 ->
    push(Bs, F, Cm, Lc, Le, Line, get(stack_trace));
push(_, _, _, _, _, _) -> true.

push(Bs, F, Cm, _, Le, Line, all) ->
    put(stack, [{Le,{Cm,extract_fnk(F),Line,Bs}}|get_stack()]);
push(Bs, F, Cm, false, Le, Line, no_tail) ->
    put(stack, [{Le,{Cm,extract_fnk(F),Line,Bs}}|get_stack()]);
push(_, _, _, _, _, _, _) -> true.

extract_fnk({Cm,Fnk,Args,No}) -> {Fnk,length(Args)};
extract_fnk(_)                -> extern.

pop(Le) -> put(stack, pop(Le, get_stack())).

pop(Le, [{L,_}|Stack]) when Le =< L ->
    pop(Le, Stack);
pop(Le, Stack) -> Stack.

get_stack() -> get(stack).

%% in_use_p(Module, CurrentModule) -> true|false
%%  Returns true if Module is found on the stack, otherwise false.

in_use_p(Mod, Mod) -> true;
in_use_p(Mod, _) ->
    case get(stack_trace) of
	false -> true;
	_ ->
	    lists:any(fun ({Le,{Cm,Func,Line,Bs}}) when Cm =:= Mod -> true;
			  (_) -> false
		      end, get_stack())
    end.

%%
%% Ask attach-process where to start new attach-process
%% 

follow_mod() ->
    case get(next_break) of
	break ->
	    follow_mod1();
	_ ->
	    false
    end.

follow_mod1() ->
    case get(attached) of
	[AttP|_] ->
	    AttP ! {self(),which_func},
	    receive
		{AttP,this_func,{Mod,Fnk}} ->
		    {Mod,Fnk,AttP};
		{'EXIT',AttP,_} ->
		    dbg_icmd:detach(AttP),
		    follow_mod1()
	    end;
	_ ->
	    false
    end.

catch_bif(0) ->
    bif;
catch_bif(Lev) when integer(Lev) ->
    catch_bif.

safe_bif(Catch, Name, As, Bs, Cm, Lc, Le, Line, F) ->
    Ref = make_ref(),
    case catch {Ref,erts_debug:apply(erlang, Name, As, clean_mfa(F))} of
	{Ref,Value} when Lc == true ->
	    {value,Value,Bs};
	{Ref,Value} ->
	    trace_fnk_ret(Le, Value, Lc),
	    {value,Value,Bs};
	{'EXIT',Reason} ->
	    case Catch of
		bif -> exit(Reason, Cm, Line, Bs);
		catch_bif -> exit(Reason)
	    end
    end.

eval_send(Line, To, Msg, Bs, Cm, F) ->
    case catch To ! Msg of
	Msg ->
	    trace_send(To,Msg),
	    {value,Msg,Bs};
	{'EXIT',{badarg,{T,M}}} ->  % Special if To is an non-existing name.
	    exit({badarg,{T,M}},Cm,Line,Bs);
	{'EXIT',Reason} ->          % Reason is badarg... if send fails !!
	    exit({badarg,F},Cm,Line,Bs);
	What ->
	    io:format('dbg_ieval:eval_send/5 - Unknown behaviour of send!\n'),
	    exit({badarg,F},Cm,Line,Bs)
    end.

%% Start tracing of messages before fetching current messages in the queue
%% to make sure that no messages are lost. 
	    
eval_receive(Msg_handler,Cs,Bs0,Cm,Lc,Le,Line,F) ->
    %% To avoid private message passing protocol between META
    %% and interpreted process.
    erlang:trace(Msg_handler,true,['receive']),

    {_,Msgs} = erlang:process_info(Msg_handler,messages),
%    io:format('Msgs = ~w~n',[Msgs]),
    case receive_clauses(Cs,Bs0,Cm,Le,Msgs) of
	nomatch ->
	    dbg_iserver_api:set_state(waiting),
	    tell_att_if_break(wait_at,Cm,Line,Le),
	    eval_receive1(Msg_handler,Cs,Bs0,Cm,Lc,Le,Line,F);
	{eval,B,Bs,Msg} ->
	    rec_mess(Msg_handler,Msg,Cm,Line,Bs0),
	    seq(B,Bs,Cm,Lc,Le,F)
    end.

eval_receive1(Msg_handler,Cs,Bs0,Cm,Lc,Le,Line,F) ->
    Msgs = do_receive(Msg_handler,Cm,Le,Line,Bs0),
%    io:format('Msgs = ~w~n',[Msgs]),
    case receive_clauses(Cs,Bs0,Cm,Le,Msgs) of
	nomatch ->
	    eval_receive1(Msg_handler,Cs,Bs0,Cm,Lc,Le,Line,F);
	{eval,B,Bs,Msg} ->
	    rec_mess(Msg_handler,Msg,Cm,Line,Bs0),
	    dbg_iserver_api:set_state(running),
	    tell_att_if_break(running,Cm,Line,Le),
	    seq(B,Bs,Cm,Lc,Le,F)
    end.

check_timeoutvalue(ToVal,_,_,_,_) when integer(ToVal), ToVal >= 0 -> true;
check_timeoutvalue(infinity,_,_,_,_)                              -> true;
check_timeoutvalue(ToVal,ToBs,To,Cm,F) ->
    Line = element(2,To),
    exit({timeout_value,F},Cm,Line,ToBs).

eval_receive(Msg_handler,Cs,0,ToExprs,ToBs,Bs0,Cm,Lc,Le,Line,0,Stamp,F) -> % Timeout after 0ms
    {_,Msgs} = erlang:process_info(Msg_handler,messages),                  % at the first call
%    io:format('Msgs = ~w~n',[Msgs]),
    case receive_clauses(Cs,Bs0,Cm,Le,Msgs) of
	{eval,B,Bs,Msg} ->
	    rec_mess_no_trace(Msg_handler,Msg,Cm,Line,Bs0),
	    seq(B,Bs,Cm,Lc,Le,F);
	nomatch ->
	    trace_received(),
	    seq(ToExprs,ToBs,Cm,Lc,Le,F)
    end;
eval_receive(Msg_handler,Cs,ToVal,ToExprs,ToBs,Bs0,Cm,Lc,Le,Line,0,Stamp,F) -> % Called first time
    erlang:trace(Msg_handler,true,['receive']),
    {_,Msgs} = erlang:process_info(Msg_handler,messages),
%    io:format('Msgs = ~w~n',[Msgs]),
    case receive_clauses(Cs,Bs0,Cm,Le,Msgs) of
	nomatch ->
	    {Stamp1,Time1} = newtime(Stamp,ToVal),
	    dbg_iserver_api:set_state(waiting),
	    tell_att_if_break(wait_after_at,Cm,Line,Le),
	    eval_receive(Msg_handler,Cs,Time1,ToExprs,ToBs,Bs0,Cm,Lc,Le,
			 Line,infinity,Stamp1,F);
	{eval,B,Bs,Msg} ->
	    rec_mess(Msg_handler,Msg,Cm,Line,Bs0),
	    seq(B,Bs,Cm,Lc,Le,F)
    end;
eval_receive(Msg_handler,Cs,ToVal,ToExprs,ToBs,Bs0,Cm,Lc,Le,Line,_,Stamp,F) -> % Now wait for 
    case do_receive(Msg_handler,ToVal,Stamp,Cm,Le,Line,Bs0) of % real timeout
	timeout ->
	    trace_received(),
	    rec_mess(Msg_handler),
	    dbg_iserver_api:set_state(running),
	    tell_att_if_break(running,Cm,Line,Le),
	    seq(ToExprs,ToBs,Cm,Lc,Le,F);
	Msgs ->
%            io:format('Msgs = ~w~n',[Msgs]),
	    case receive_clauses(Cs,Bs0,Cm,Le,Msgs) of
		nomatch ->
		    {Stamp1,Time1} = newtime(Stamp,ToVal),
		    eval_receive(Msg_handler,Cs,Time1,ToExprs,ToBs,Bs0,Cm,Lc,Le,Line,
				 infinity,Stamp1,F);
		{eval,B,Bs,Msg} ->
		    rec_mess(Msg_handler,Msg,Cm,Line,Bs0),
		    dbg_iserver_api:set_state(running),
		    tell_att_if_break(running,Cm,Line,Le),
		    seq(B,Bs,Cm,Lc,Le,F)
	    end
    end.

tell_att_if_break(running,_,_,_) ->
    case get(next_break) of
	break -> dbg_icmd:tell_attached({self(),running});
	_     -> ok
    end;
tell_att_if_break(RecAt,Cm,Line,Le) ->
    case get(next_break) of
	break -> dbg_icmd:tell_attached({self(),RecAt,Cm,Line,Le});
	_     -> ok
    end.

do_receive(Msg_handler,Cm,Le,Line,Bs) ->
    receive
	{trace,Msg_handler,'receive',Msg} ->
	    [Msg];
	{cmd,Cmd,Args} ->
	    dbg_icmd:cmd_rec(Cmd, Args, Bs),
	    do_receive(Msg_handler,Cm,Le,Line,Bs);
	{break_msg,Type,Data} ->
	    dbg_icmd:break_msg(Type,Data),
	    do_receive(Msg_handler,Cm,Le,Line,Bs);
	{attach,Msg_handler,AttPid} ->
	    dbg_icmd:attach(AttPid,Cm,Line),
	    AttPid ! {self(),wait_at,Cm,Line,Le},
	    do_receive(Msg_handler,Cm,Le,Line,Bs);

	%% Handle removal of interpreted code.
	{old_code, Module} ->
	    case in_use_p(Module, Cm) of
		%% A call to the Module is on the stack (or unknown), so we must terminate.
		true ->
%		    dbg_icmd:tell_attached({self(),old_code,Module}),
		    exit(get(self), kill),
		    exit(old_code, Cm, Line, Bs);
		false ->
		    erase([Module|db]),
		    erase(cache),
		    do_receive(Msg_handler,Cm,Le,Line,Bs)
	    end;


	%%FIXME: Handle new_code.
	{new_code, Module} ->
	    io:format("NYI: Handling of {new_code,Module} in: do_receive,Module:~p~n",[Module]),
	    do_receive(Msg_handler,Cm,Le,Line,Bs);



	{'EXIT',Msg_handler,Reason} ->
	    exit(Msg_handler,Reason,Cm,Line,Bs);
	{'EXIT',Pid,Reason} ->
	    case dbg_icmd:attached_p(Pid) of
		true ->
		    dbg_icmd:detach(Pid),
		    do_receive(Msg_handler,Cm,Le,Line,Bs);
		_ ->
		    exit(Reason,Cm,Line,Bs)
	    end
    end.

do_receive(Msg_handler,Time,Stamp,Cm,Le,Line,Bs) ->
    receive
	{trace,Msg_handler,'receive',Msg} ->
	    [Msg];
	{cmd,timeout,_} ->
	    timeout;
	{cmd,Cmd,Args} ->
	    dbg_icmd:cmd_rec(Cmd, Args, Bs),
	    {Stamp1,Time1} = newtime(Stamp,Time),
	    do_receive(Msg_handler,Time1,Stamp1,Cm,Le,Line,Bs);
	{break_msg,Type,Data} ->
	    dbg_icmd:break_msg(Type,Data),
	    {Stamp1,Time1} = newtime(Stamp,Time),
	    do_receive(Msg_handler,Time1,Stamp1,Cm,Le,Line,Bs);
	{attach,Msg_handler,AttPid} ->
	    dbg_icmd:attach(AttPid,Cm,Line),
	    AttPid ! {self(),wait_after_at,Cm,Line,Le},
	    {Stamp1,Time1} = newtime(Stamp,Time),
	    do_receive(Msg_handler,Time1,Stamp1,Cm,Le,Line,Bs);

		%% Handle removal of interpreted code.
	{old_code, Module} ->
	    case in_use_p(Module, Cm) of
		%% A call to the Module is on the stack (or unknown), so we must terminate.
		true ->
%		    dbg_icmd:tell_attached({self(),old_code,Module}),
		    exit(get(self), kill),
		    exit(old_code, Cm, Line, Bs);
		false ->
		    erase([Module|db]),
		    erase(cache),
		    do_receive(Msg_handler,Time,Stamp,Cm,Le,Line,Bs)
	    end;

	%%FIXME: Handle new_code.
	{new_code, Module} ->
	    io:format("NYI: Handling of {new_code,Module} in: do_receive,Module:~p~n",[Module]),	    
	    do_receive(Msg_handler,Time,Stamp,Cm,Le,Line,Bs);

	{'EXIT',Msg_handler,Reason} ->
	    exit(Msg_handler,Reason,Cm,Line,Bs);
	{'EXIT',Pid,Reason} ->
	    case dbg_icmd:attached_p(Pid) of
		true ->
		    dbg_icmd:detach(Pid),
		    {Stamp1,Time1} = newtime(Stamp,Time),
		    do_receive(Msg_handler,Time1,Stamp1,Cm,Le,Line,Bs);
		_ ->
		    exit(Reason,Cm,Line,Bs)
	    end
    after Time ->
	    timeout
    end.

newtime(Stamp,infinity) ->
    {Stamp,infinity};
newtime(Stamp,Time) ->
    {Stamp1,_} = statistics(wall_clock),
    case Time - (Stamp1 - Stamp) of
	NewTime when NewTime > 0 ->
	    {Stamp1,NewTime};
	_ ->
	    {Stamp1,0}
    end.

rec_mess(Msg_handler,Msg,Cm,Line,Bs) ->
    erlang:trace(Msg_handler,false,['receive']),
    flush_traces(Msg_handler),
    Msg_handler ! {sys,self(),{'receive',Msg}},
    rec_ack(Msg_handler,Cm,Line,Bs).

rec_mess(Msg_handler) ->
    erlang:trace(Msg_handler,false,['receive']),
    flush_traces(Msg_handler).

rec_mess_no_trace(Msg_handler,Msg,Cm,Line,Bs) ->
    Msg_handler ! {sys,self(),{'receive',Msg}},
    rec_ack(Msg_handler,Cm,Line,Bs).

rec_ack(Msg_handler,Cm,Line,Bs) ->
    receive
	{Msg_handler,rec_acked} ->
	    true;
	{'EXIT',Msg_handler,Reason} ->
	    exit(Msg_handler,Reason,Cm,Line,Bs)
    end.

flush_traces(Msg_handler) ->
    receive
	{trace,Msg_handler,'receive',_} ->
	    flush_traces(Msg_handler)
    after 0 ->
	    true
    end.

%% eval_list(ExpressionList, Bindings, CurrentModule, Level, Line, Function)
%%  Evaluate a list of expressions "in parallel" at the same level.

eval_list(Es, Bs, Cm, Le, Line, F) ->
    eval_list(Es, [], Bs, Bs, Cm, Le, Line, F).

eval_list([E|Es], Vs, BsOrig, Bs0, Cm, Le, Line, F) ->
    {value,V,Bs1} = expr(E, BsOrig, Cm, false, Le, F),
    eval_list(Es, [V|Vs], BsOrig, merge_bindings(Bs1, Bs0, Cm, Line, F),
	      Cm, Le, Line, F);
eval_list([], Vs, _, Bs, _, _, _, _) ->
    {reverse(Vs,[]),Bs}.

%% if_clauses(Clauses, Bindings, CurrentModule, LastCall, Line, Function)

if_clauses([{clause,_,[],G,B}|Cs], Bs, Cm, Lc, Le, Line, F) ->
    case guard(G, Bs) of
	true ->
	    seq(B, Bs, Cm, Lc, Le, F);
	false ->
	    if_clauses(Cs, Bs, Cm, Lc, Le, Line, F)
    end;
if_clauses([], Bs, Cm, Lc, Le, Line, F) ->
    exit({if_clause,F},Cm,Line,Bs).

%% case_clauses(Value, Clauses, Bindings, CurrentModule, LastCall, Line, Function)

case_clauses(Val, [{clause,_,[P],G,B}|Cs], Bs0, Cm, Lc, Le, Line, F) ->
    case match(P, Val, Bs0) of
	{match,Bs} ->
	    case guard(G, Bs) of
		true ->
		    seq(B, Bs, Cm, Lc, Le, F);
		false ->
		    case_clauses(Val, Cs, Bs0, Cm, Lc, Le, Line, F)
	    end;
	nomatch ->
	    case_clauses(Val, Cs, Bs0, Cm, Lc, Le, Line, F)
    end;
case_clauses(Val, [], Bs, Cm, Lc, Le, Line, F) ->
    exit({{case_clause,Val},F},Cm,Line,Bs).

fnk_clauses(F,[{clause,_,Pars,G,B}|Cs], Args, Cm, Lc, Le, CallM, Line, Bs0) ->
    case head_match(Pars, Args, []) of
	{match,Bs} ->
	    case guard(G, Bs) of
		true ->
		    seq(B,Bs,Cm,Lc,Le,{Cm,F,Args,make_ref()});
		false ->
		    fnk_clauses(F,Cs,Args,Cm,Lc,Le,CallM,Line,Bs0)
	    end;
	nomatch ->
	    fnk_clauses(F,Cs,Args,Cm,Lc,Le,CallM,Line,Bs0)
    end;
fnk_clauses(F,[],Args,Cm,Lc,Le,CallM,Line,Bs0) ->
    exit({function_clause,{Cm,F,Args,make_ref()}},CallM,Line,Bs0).

in_fnk({M,F,A,_}) -> [{M,F,A}];
in_fnk({M,F,A}=MFA) -> [MFA];
in_fnk(Fnk) -> Fnk.

clean_mfa({M,F,A,_}) -> {M,F,A};
clean_mfa(Other) -> Other.

receive_clauses(Cs,Bs0,Cm,Le,[Msg|Msgs]) ->
    case rec_clauses(Cs,Bs0,Cm,Le,Msg,1) of
	nomatch ->
	    receive_clauses(Cs,Bs0,Cm,Le,Msgs);
	{eval,B,Bs} ->
	    {eval,B,Bs,Msg}
    end;
receive_clauses(_,_,_,_,[]) ->
    nomatch.

rec_clauses([{clause,_,Pars,G,B}|Cs],Bs0,Cm,Le,Msg,Clause) when integer(Clause) ->
    case rec_match(Pars,Msg,Bs0) of
	{match,Bs} ->
	    case guard(G, Bs) of
		true ->
		    trace_received(Msg),
		    {eval,B,Bs};
		false ->
		    rec_clauses(Cs,Bs0,Cm,Le,Msg,Clause+1)
	    end;
	nomatch ->
	    rec_clauses(Cs,Bs0,Cm,Le,Msg,Clause+1)
    end;
rec_clauses([],_,_,_,_,_) ->
    nomatch.


%% guard(GuardTests, Bindings)
%%  Evaluate a list of guards.

guard([], Bs) -> true;
guard(Gs, Bs) -> or_guard(Gs, Bs).
    
or_guard([G|Gs], Bs) ->
    %% Short-circuit OR.
    case and_guard(G, Bs) of
	true -> true;
	false -> or_guard(Gs, Bs)
    end;
or_guard([], Bs) -> false.

and_guard([G|Gs], Bs) ->
    %% Short-circuit AND.
    case catch guard_expr(G, Bs) of
	{value,true} -> and_guard(Gs, Bs);
	Other -> false
    end;
and_guard([], _) -> true.

guard_exprs([A0|As0], Bs) ->
    {value,A} = guard_expr(A0, Bs),
    {values,As} = guard_exprs(As0, Bs),
    {values,[A|As]};
guard_exprs([], Bs) ->
    {values,[]}.

guard_expr(self, Bs) ->
    {value,get(self)};
guard_expr({safe_bif,Line,Mod,Func,As0}, Bs) ->
    {values,As} = guard_exprs(As0, Bs),
    {value,apply(Mod, Func, As)};
guard_expr({var,Line,V}, Bs) ->
    {value,_} = binding(V, Bs);
guard_expr({value,Line,Val}, Bs) ->
    {value,Val};
guard_expr({cons,Line,H0,T0}, Bs) ->
    {value,H} = guard_expr(H0, Bs),
    {value,T} = guard_expr(T0, Bs),
    {value,[H|T]};
guard_expr({tuple,Line,Es0}, Bs) ->
    {values,Es} = guard_exprs(Es0, Bs),
    {value,list_to_tuple(Es)};
guard_expr({bin,Line,Flds}, Bs) ->
    {value,V,_Bs} = eval_bits:expr_grp(Flds, Bs,
				       fun(E, B) ->
					       {value,V} = guard_expr(E, B),
					       {value,V,B}
				       end, [], false),
    {value,V}.


%% match(Pattern, Term, Bindings)
%%  Try to match Pattern against Term with the current bindings.
%%  Returns:
%%	{match,NewBindings}
%%	nomatch

match(Pat, Term, Bs) ->
    catch match1(Pat, Term, Bs).

match1({value,Line,V}, V, Bs) ->
    {match,Bs};
match1({var,_,'_'}, Term, Bs) ->		%Anonymous variable matches
    {match,add_anon(Term,Bs)};			% everything, save it anyway.
match1({var,_,Name}, Term, Bs) ->
    case binding(Name, Bs) of
	{value,Term} ->
	    {match,Bs};
	{value,V} ->
	    throw(nomatch);
	unbound ->
	    {match,[{Name,Term}|Bs]}            %Add the new binding
    end;
match1({match,Line,Pat1,Pat2}, Term, Bs0) ->
    {match,Bs1} = match1(Pat1, Term, Bs0),
    match1(Pat2, Term, Bs1);
match1({cons,_,H,T}, [H1|T1], Bs0) ->
    {match,Bs} = match1(H, H1, Bs0),
    match1(T, T1, Bs);
match1({tuple,_,Elts}, Tuple, Bs) when tuple(Tuple), length(Elts) =:= size(Tuple) ->
    match_tuple(Elts, Tuple, 1, Bs);
match1({bin,_,Fs}, B, Bs0) when binary(B) ->
    eval_bits:match_bits(Fs, B, Bs0,
			 fun(L,R,Bs) -> match1(L,R,Bs) end,
			 fun(E,Bs) -> expr(E,Bs,foo,foo,foo,foo) end,
			 false);
match1(_, _, _) ->
    throw(nomatch).

match_tuple([E|Es], Tuple, I, Bs0) ->
    {match,Bs} = match1(E, element(I, Tuple), Bs0),
    match_tuple(Es, Tuple, I+1, Bs);
match_tuple([], _, _, Bs) ->
    {match,Bs}.

head_match([Par|Pars], [Arg|Args], Bs0) ->
    case match(Par, Arg, Bs0) of
	{match,Bs} -> head_match(Pars, Args, Bs);
	nomatch -> nomatch
    end;
head_match([], [], Bs) -> {match,Bs}.

rec_match([Par],Msg,Bs0) ->
    match(Par,Msg,Bs0).


%% binding(Name, Bindings)
%% add_anon(Value, Bindings)

%% Try match bindings in the function head instead of
%% recursion on every Binding. Recursion after the 6:th element.

binding(Name, [{Name,Val}|_]) ->
    {value,Val};
binding(Name, [_,{Name,Val}|_]) ->
    {value,Val};
binding(Name, [_,_,{Name,Val}|_]) ->
    {value,Val};
binding(Name, [_,_,_,{Name,Val}|_]) ->
    {value,Val};
binding(Name, [_,_,_,_,{Name,Val}|_]) ->
    {value,Val};
binding(Name, [_,_,_,_,_,{Name,Val}|_]) ->
    {value,Val};
binding(Name, [_,_,_,_,_,_|Bs]) ->
    binding(Name, Bs);
binding(Name, [_,_,_,_,_|Bs]) ->
    binding(Name, Bs);
binding(Name, [_,_,_,_|Bs]) ->
    binding(Name, Bs);
binding(Name, [_,_,_|Bs]) ->
    binding(Name, Bs);
binding(Name, [_,_|Bs]) ->
    binding(Name, Bs);
binding(Name, [_|Bs]) ->
    binding(Name, Bs);
binding(_, []) ->
    unbound.

add_anon(Val, [{'_',_}|Bs]) ->
    [{'_',Val}|Bs];
add_anon(Val, [B1,{'_',_}|Bs]) ->
    [B1,{'_',Val}|Bs];
add_anon(Val, [B1,B2,{'_',_}|Bs]) ->
    [B1,B2,{'_',Val}|Bs];
add_anon(Val, [B1,B2,B3,{'_',_}|Bs]) ->
    [B1,B2,B3,{'_',Val}|Bs];
add_anon(Val, [B1,B2,B3,B4,{'_',_}|Bs]) ->
    [B1,B2,B3,B4,{'_',Val}|Bs];
add_anon(Val, [B1,B2,B3,B4,B5,{'_',_}|Bs]) ->
    [B1,B2,B3,B4,B5,{'_',Val}|Bs];
add_anon(Val, [B1,B2,B3,B4,B5,B6|Bs]) ->
    [B1,B2,B3,B4,B5,B6|add_anon(Val,Bs)];
add_anon(Val, [B1,B2,B3,B4,B5|Bs]) ->
    [B1,B2,B3,B4,B5|add_anon(Val,Bs)];
add_anon(Val, [B1,B2,B3,B4|Bs]) ->
    [B1,B2,B3,B4|add_anon(Val,Bs)];
add_anon(Val, [B1,B2,B3|Bs]) ->
    [B1,B2,B3|add_anon(Val,Bs)];
add_anon(Val, [B1,B2|Bs]) ->
    [B1,B2|add_anon(Val,Bs)];
add_anon(Val, [B1|Bs]) ->
    [B1|add_anon(Val,Bs)];
add_anon(Val, []) ->
    [{'_',Val}].

%% merge_bindings(Bindings1, Bindings2, Cm, Line, Function)
%%  Merge bindings detecting bad matches. 
%%  Special case '_', save the new one !!!
%%  Bindings1 is the newest bindings.

merge_bindings(Bs, Bs, _, _, _) ->
    Bs;                                         %Identical bindings
merge_bindings([{Name,V}|B1s], B2s, Cm, Line, F) ->
    case binding(Name, B2s) of
	{value,V} ->				%Already there, and the SAME
	    merge_bindings(B1s, B2s, Cm, Line, F);
	{value,V1} when Name == '_' ->		%	but anonym different
	    B2s1 = lists:keydelete('_',1,B2s),
	    [{Name,V}|merge_bindings(B1s, B2s1, Cm, Line, F)];
	{value,V1} ->				%	but different
	    exit({{badmatch,V},F},Cm,Line,B2s);
	unbound ->				%Not there, add it
	    [{Name,V}|merge_bindings(B1s, B2s, Cm, Line, F)]
    end;
merge_bindings([], B2s, _, _, _) ->
    B2s.
%% add_bindings(Bindings1, Bindings2)
%%  Add Bindings1 to Bindings2. Bindings in
%%  Bindings1 hides bindings in Bindings2.
%%  Used in list comprehensions (and funs).

add_bindings([{Name,V}|Bs], ToBs0) ->
    ToBs = add_binding(Name, V, ToBs0),
    add_bindings(Bs, ToBs);
add_bindings([], ToBs) ->
    ToBs.

add_binding(N, Val, [{N,_}|Bs]) ->
    [{N,Val}|Bs];
add_binding(N, Val, [B1,{N,_}|Bs]) ->
    [B1,{N,Val}|Bs];
add_binding(N, Val, [B1,B2,{N,_}|Bs]) ->
    [B1,B2,{N,Val}|Bs];
add_binding(N, Val, [B1,B2,B3,{N,_}|Bs]) ->
    [B1,B2,B3,{N,Val}|Bs];
add_binding(N, Val, [B1,B2,B3,B4,{N,_}|Bs]) ->
    [B1,B2,B3,B4,{N,Val}|Bs];
add_binding(N, Val, [B1,B2,B3,B4,B5,{N,_}|Bs]) ->
    [B1,B2,B3,B4,B5,{N,Val}|Bs];
add_binding(N, Val, [B1,B2,B3,B4,B5,B6|Bs]) ->
    [B1,B2,B3,B4,B5,B6|add_binding(N,Val,Bs)];
add_binding(N, Val, [B1,B2,B3,B4,B5|Bs]) ->
    [B1,B2,B3,B4,B5|add_binding(N,Val,Bs)];
add_binding(N, Val, [B1,B2,B3,B4|Bs]) ->
    [B1,B2,B3,B4|add_binding(N,Val,Bs)];
add_binding(N, Val, [B1,B2,B3|Bs]) ->
    [B1,B2,B3|add_binding(N,Val,Bs)];
add_binding(N, Val, [B1,B2|Bs]) ->
    [B1,B2|add_binding(N,Val,Bs)];
add_binding(N, Val, [B1|Bs]) ->
    [B1|add_binding(N,Val,Bs)];
add_binding(N, Val, []) ->
    [{N,Val}].

format_args(As) -> lists:flatten(format_args1(As)).

format_args1([A]) ->
    [io_lib:format('~p',[A])];
format_args1([A|As]) ->
    [io_lib:format('~p',[A]),$,|format_args1(As)];
format_args1([]) ->
    [].

trace_p() -> get(trace).

trace_receive(Le, TimeoutP) ->
    Tail = case TimeoutP of
	       true -> "with timeout ";
	       _    -> ""
	   end,
    case trace_p() of
	true -> trace_out(io_lib:format("++ (~w) receive " ++ Tail,[Le]));
	_    -> true
    end.

trace_received() ->
    case trace_p() of
	true -> trace_out(io_lib:format("~n",[]));
	_    -> true
    end.

trace_received(Msg) ->
    case trace_p() of
	true -> trace_out(io_lib:format("~n<== ~p~n",[Msg]));
	_    -> true
    end.

trace_fnk_call(Le,Line,M,Fu,As) ->
    case trace_p() of
	true -> trace_out(io_lib:format("++ (~w) <~w> ~w:~w(~s)~n",
					[Le,Line,M,Fu,format_args(As)]));
	_    -> true
    end.

trace_fnk_ret(Le,Val,false) ->
    case trace_p() of
	true -> trace_out(io_lib:format("-- (~w) ~p~n", [Le,Val]));
	_    -> true
    end;
trace_fnk_ret(_,_,_) ->
    true.

trace_bif_call(Le,Line,Name,As) ->
    case trace_p() of
	true -> trace_out(io_lib:format("++ (~w) <~w> ~w(~s)~n",
					[Le,Line,Name,format_args(As)]));
	_    -> true
    end.

trace_send(To,Msg) ->
    case trace_p() of
	true -> trace_out(io_lib:format("==> ~w : ~p~n",[To,Msg]));
	_    -> true
    end.

trace_out(String) ->
    dbg_icmd:tell_attached({self(),trace,String}).
