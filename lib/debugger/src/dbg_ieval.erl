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
-module(dbg_ieval).

%% External exports
-export([eval_function/4, eval_expr/6,
	 exit/4, exit/5, do_put_error/4,
	 pop/1,
	 in_use_p/2]).
-export([trace/2]).
-export([init_catch_lev/0, exit_catch_lev/0]).

%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% eval_function(Mod, Func, Args, Le) -> {value, Val, Bs}
%%   Mod = Func = atom()
%%   Args = [term()]
%%   Le = integer()
%%--------------------------------------------------------------------
eval_function(Mod, Func, Args, Le) ->
    catch eval_function(Mod, Func, Args, [],
			Mod, false, Le, extern, -1, local).

%%--------------------------------------------------------------------
%% eval_expr(Expr, Bs, Cm, Lc, Le, F) -> {value, Value, Bs}
%%   Bs = [{Var, Value}]           Bindings
%%   Cm = atom()                   Module
%%   Lc = boolean()                Local call
%%   F = extern | {M,F,A,No(de?)}  Function
%% Evalute a shell expression in the real process.
%%--------------------------------------------------------------------
eval_expr(Expr, Bs, Cm, Lc, Le, F) ->
    Debugged = get(self),
    Debugged ! {sys, self(), {eval, Expr, Bs}},
    LineNo = element(2, Expr),
    dbg_imeta:main_meta_loop(Debugged, Bs, Le, Lc, Cm, LineNo, F).

%%--------------------------------------------------------------------
%% format_args(As) -> string()
%%   As = [term()]
%%--------------------------------------------------------------------
format_args(As) ->
    lists:flatten(format_args1(As)).

format_args1([A]) ->
    [io_lib:format('~p', [A])];
format_args1([A|As]) ->
    [io_lib:format('~p', [A]),$,|format_args1(As)];
format_args1([]) ->
    [].

%%--------------------------------------------------------------------
%% exit(Reason, Cm, LineNo, Bs)
%% exit(Debugged, Reason, Cm, LineNo, Bs)
%% do_put_error(Reason, Cm, LineNo, Bs)
%%   Debugged = pid()
%%   Reason = {What,Where} | term()
%%     Where = tuple()
%% Store the error reason and position if not stored already.
%% Terminate according to type of exit.
%%--------------------------------------------------------------------
exit({What,Where}, Cm, LineNo, Bs) when tuple(Where) ->
    Reason = {What,in_fnk(Where)},
    put_error(Reason, Cm, LineNo, Bs),
    exit(Reason);
exit(Reason, Cm, LineNo, Bs) ->
    put_error(Reason, Cm, LineNo, Bs),
    exit(Reason).

exit(Debugged, Reason, Cm, LineNo, Bs) ->
    put_error(Reason, Cm, LineNo, Bs),
    exit({Debugged, Reason}).
    
put_error(Reason, Cm, LineNo, Bs) ->
    case get(error) of
	none when LineNo=/=-1 ->
	    do_put_error(Reason, Cm, LineNo, Bs);
	{R,_,_,_} when R=/=Reason, LineNo=/=-1 ->
	    do_put_error(Reason, Cm, LineNo, Bs);
	{Reason,{C,_},_,_} when C=/=Cm, LineNo=/=-1 ->
	    do_put_error(Reason, Cm, LineNo, Bs);
	{Reason,{Cm,L},_,_} when L=/=LineNo, LineNo=/=-1 ->
	    do_put_error(Reason, Cm, LineNo, Bs);
	_ ->
	    ignore
    end.

do_put_error(Reason, Cm, LineNo, Bs) ->
    BinStack = term_to_binary(get(stack)),
    put(error, {Reason, {Cm,LineNo}, Bs, BinStack}).

%%--------------------------------------------------------------------
%% pop(Le)
%% Hold a stack which can be inspected (at break and exit).
%%
%% The push-function is conditional.
%% It determines whether or not to push stuff on the stack
%% by looking at the stack_trace option stored in the process 
%% dictonary
%%--------------------------------------------------------------------

pop(Le) -> put(stack, pop(Le, get(stack))).

pop(Le, [{L,_}|Stack]) when Le=<L ->
    pop(Le, Stack);
pop(Le, Stack) -> Stack.

push(Bs, F, Cm, Lc, Le, LineNo) when LineNo>0 ->
    push(Bs, F, Cm, Lc, Le, LineNo, get(stack_trace));
push(_, _, _, _, _, _) -> ignore.

push(Bs, F, Cm, _Lc, Le, LineNo, all) ->
    put(stack, [{Le,{Cm,extract_fnk(F),LineNo,Bs}}|get(stack)]);
push(Bs, F, Cm, false, Le, LineNo, no_tail) ->
    put(stack, [{Le,{Cm,extract_fnk(F),LineNo,Bs}}|get(stack)]);
push(_Bs, _F, _Cm, _Lc, _Le, _LineNo, _Flag) -> ignore.

extract_fnk({Cm,Fnk,Args,No}) -> {Fnk,length(Args)};
extract_fnk(_)                -> extern.

%%--------------------------------------------------------------------
%% in_use_p(Mod, Cm) -> boolean()
%% Returns true if Mod is found on the stack, otherwise false.
%%--------------------------------------------------------------------
in_use_p(Mod, Mod) -> true;
in_use_p(Mod, _Cm) ->
    case get(stack_trace) of
	false -> true;
	_ -> %  all | no_tail
	    lists:any(fun({Le,{Cm,Func,LineNo,Bs}}) when Cm=:=Mod -> true;
			 (_) -> false
		      end,
		      get(stack))
    end.

%%--------------------------------------------------------------------
%% trace(What, Args)
%%   What = send | receivex | received | call | return | bif
%%   Args depends on What, see the code.
%%--------------------------------------------------------------------
trace(What, Args) ->
    case get(trace) of
	true ->
	    Str = case What of
		      send ->
			  {To,Msg} = Args,
			  io_lib:format("==> ~w : ~p~n", [To, Msg]);
		      receivex ->
			  {Le, TimeoutP} = Args,
			  Tail = case TimeoutP of
				     true -> "with timeout~n";
				     false -> "~n"
				 end,
			  io_lib:format("++ (~w) receive " ++ Tail, [Le]);
		      
		      received when Args==null ->
			  io_lib:format("~n", []);
		      received -> % Args=Msg
			  io_lib:format("~n<== ~p~n", [Args]);
		      
		      call ->
			  {Le,Li,M,Fu,As} = Args,
			  io_lib:format("++ (~w) <~w> ~w:~w(~s)~n",
					[Le, Li, M, Fu, format_args(As)]);
		      return ->
			  {Le,Val,false} = Args,
			  io_lib:format("-- (~w) ~p~n", [Le, Val]);
		      
		      
		      bif ->
			  {Le,LineNo,Name,As} = Args,
			  io_lib:format("++ (~w) <~w> ~w(~s)~n",
					[Le, LineNo, Name, format_args(As)])
		  end,
	    
	    dbg_icmd:tell_attached({trace_output, Str});
	false -> ignore
    end.


%%====================================================================
%% Internal functions
%%====================================================================

eval_function(_Mod, Fun, As0, Bs, Cm, Lc, Le, F, LineNo, _Called)
  when is_function(Fun) ->
    push(Bs, F, Cm, Lc, Le, LineNo),
    case lambda(Fun, As0) of
	undef ->
	    exit({undef,[{Fun,As0}|in_fnk(F)]}, Cm, LineNo, Bs);
	badfun ->
	    exit({{badfun,Fun},F}, Cm, LineNo, Bs);
	badarity ->
	    exit({badarity,[{Fun,As0}|in_fnk(F)]}, Cm, LineNo, Bs);
	not_interpreted ->
	    Debugged = get(self),
	    Debugged ! {sys, self(), {catch_apply(get_catch_lev()),
				      erlang, apply, [Fun,As0]}},
	    %% Enter meta-wait state
	    {value, Val, _Bs} =
		dbg_imeta:main_meta_loop(Debugged,
					 Bs, Le+1, false, Cm, LineNo, F),
	    pop(Le),
	    {value, Val, Bs};
	{Cs, Mod, Name, As} when Lc==false ->
	    {value, Val, _Bs} =
		fnk_clauses(Name,Cs,As,Mod,true,Le+1,Cm,LineNo,Bs),
	    trace(return, {Le,Val,Lc}),
	    pop(Le),
	    {value, Val, Bs};
	{Cs, Mod, Name, As} ->
	    fnk_clauses(Name, Cs, As, Mod, Lc, Le+1, Cm, LineNo,Bs)
    end;
eval_function(Mod, Name, As, Bs, Cm, Lc, Le, F, LineNo, Called) ->
    push(Bs, F, Cm, Lc, Le, LineNo),
    case function(Mod, Name, As, Called) of

	%% Continue evaluation in meta process
	Cs when list(Cs), Lc==false ->
	    {value, Val, _} =
		fnk_clauses(Name, Cs, As, Mod, true, Le+1, Cm, LineNo, Bs),
	    trace(return, {Le,Val,Lc}),
	    pop(Le),
	    {value, Val, Bs};
	Cs when list(Cs) ->
	    fnk_clauses(Name, Cs, As, Mod, Lc, Le+1, Cm, LineNo, Bs);

	%% Continue evaluation in non-interpreted code
	not_interpreted ->
	    Debugged = get(self),
	    Debugged ! {sys, self(), {catch_apply(get_catch_lev()),
				      Mod, Name, As}},
	    {value, Val, _Bs} =
		dbg_imeta:main_meta_loop(Debugged,
					 Bs, Le+1, false, Cm, LineNo, F),
	    pop(Le),
	    {value, Val, Bs};
	undef ->
	    exit({undef,[{Mod,Name,As}|in_fnk(F)]}, Cm, LineNo, Bs)
    end.

lambda(Fun, As0) when is_function(Fun) ->
    {module, Mod} = erlang:fun_info(Fun, module),
    case fun_call_clauses(Mod, Fun) of
	{Name, Arity, Cs} ->
	    {env, Env} = erlang:fun_info(Fun, env),
	    case As0 ++ Env of
		As when length(As) =:= Arity ->
		    {Cs, Mod, Name, As};
		As ->
		    badarity
	    end;
	Error -> Error
    end;
lambda(Fun, As) -> badfun.
	    
fun_call_clauses(Mod, Fun) ->
    {new_index, Index} = erlang:fun_info(Fun, new_index),
    {new_uniq, Uniq} = erlang:fun_info(Fun, new_uniq),
    case fun_call_clauses_1(Mod, {'fun',Mod,Index,Uniq}) of
	Data when is_tuple(Data) -> Data;
	Error -> Error
    end.

fun_call_clauses_1(Mod, Key) ->
    case cached(Key) of
	false ->
	    case db_ref(Mod) of
		not_found -> not_interpreted;
		DbRef ->
		    case dbg_idb:lookup(DbRef, Key) of
			{ok, Data} ->
			    cache(Key, Data),
			    Data;
			Error ->
			    case dbg_idb:lookup(DbRef, module) of
				{ok, _} -> undef;
				Error -> not_interpreted
			    end
		    end
	    end;
	FunData when tuple(FunData) ->
	    FunData
    end.

function(Mod, Name, Args, local) ->
    Arity = length(Args),
    Key = {Mod,Name,Arity},
    case cached(Key) of
	false ->
	    DbRef = db_ref(Mod),
	    case dbg_idb:match_object(DbRef, {{Mod,Name,Arity,'_'},'_'}) of
		[{{Mod,Name,Arity,Exp},Clauses}] ->
		    cache(Key, {Exp,Clauses}),
		    Clauses;
		Other -> undef
	    end;
	{_Exp,Cs} -> Cs
    end;
function(Mod, Name, Args, extern) ->
    Arity = length(Args),
    Key = {Mod,Name,Arity},
    case cached(Key) of
	false ->
	    case db_ref(Mod) of
		not_found -> not_interpreted;
		DbRef ->
		    case dbg_idb:lookup(DbRef, {Mod,Name,Arity,true}) of
			{ok,Data} ->
			    cache(Key, {true,Data}),
			    Data;
			Error ->
			    case dbg_idb:lookup(DbRef, module) of
				{ok,_} -> undef;
				Error -> not_interpreted
			    end
		    end
	    end;
	{true,Cs} ->
	    Cs;
	{false,Cs} ->
	    undef
    end.

db_ref(Mod) ->
    case get([Mod|db]) of
	undefined ->
	    Int = get(int),
	    case dbg_iserver:call(Int, {get_module_db, Mod, get(self)}) of
		not_found ->
		    not_found;
		ModDb ->
		    Node = node(Int),
		    DbRef = if
				Node/=node() -> {Node, ModDb};
				true -> ModDb
			    end,
		    put([Mod|db], DbRef),
		    DbRef
	    end;
	DbRef ->
	    DbRef
    end.


cache(Key, Data) ->
    put(cache, lists:sublist([{Key, Data} | get(cache)], 5)).
	    
cached(Key) ->
    case lists:keysearch(Key, 1, get(cache))  of
	{value, {Key, Data}} -> Data;
	false -> false
    end.

fnk_clauses(F,[{clause,_,Pars,G,B}|Cs],Args,Cm,Lc,Le,CallM,LineNo,Bs0) ->
    case head_match(Pars,Args,[]) of
	{match,Bs} ->
	    case guard(G,Bs) of
		true ->
		    seq(B,Bs,Cm,Lc,Le,{Cm,F,Args,make_ref()});
		false ->
		    fnk_clauses(F,Cs,Args,Cm,Lc,Le,CallM,LineNo,Bs0)
	    end;
	nomatch ->
	    fnk_clauses(F,Cs,Args,Cm,Lc,Le,CallM,LineNo,Bs0)
    end;
fnk_clauses(F,[],Args,Cm,Lc,Le,CallM,LineNo,Bs0) ->
    exit({function_clause,{Cm,F,Args,make_ref()}},CallM,LineNo,Bs0).

seq([E],Bs0,Cm,Lc,Le,F) ->
    case dbg_icmd:cmd(E,Bs0,Cm,Le,F) of
	{skip,Bs} ->
	    {value,skipped,Bs};
	Bs ->
	    expr(E,Bs,Cm,Lc,Le,F)
    end;
seq([E|Es],Bs0,Cm,Lc,Le,F) ->
    case dbg_icmd:cmd(E,Bs0,Cm,Le,F) of
	{skip,Bs} ->
	    seq(Es,Bs,Cm,Lc,Le,F);
	Bs1 ->
	    {value,_,Bs} = expr(E,Bs1,Cm,false,Le,F),
	    seq(Es,Bs,Cm,Lc,Le,F)
    end;
seq([],Bs,Cm,_,_,_) ->
    {value,true,Bs}.

%% Variable
expr({var,LineNo,V},Bs,Cm,_,_,F) ->
    case binding(V,Bs) of
	{value,Val} ->
	    {value,Val,Bs};
	unbound ->
	    exit({{unbound,V},F},Cm,LineNo,Bs)
    end;

expr({value,LineNo,Val},Bs,_,_,_,_) ->
    {value,Val,Bs};
expr({value,Val},Bs,_,_,_,_) -> % Special case straight values
    {value,Val,Bs};

%% List
expr({cons,LineNo,H0,T0},Bs0,Cm,_,Le,F) ->
    {value,H,Bs1} = expr(H0,Bs0,Cm,false,Le,F),
    {value,T,Bs2} = expr(T0,Bs0,Cm,false,Le,F),
    {value,[H|T],merge_bindings(Bs2,Bs1,Cm,LineNo,F)};

%% Tuple
expr({tuple,LineNo,Es0},Bs0,Cm,_,Le,F) ->
    {Vs,Bs} = eval_list(Es0,Bs0,Cm,Le,LineNo,F),
    {value,list_to_tuple(Vs),Bs};

%% A block of statements
expr({block,_,Es},Bs,Cm,Lc,Le,F) ->
    seq(Es,Bs,Cm,Lc,Le,F);

%% Catch statement
expr({'catch',_,Expr},Bs0,Cm,_,Le,F) ->
    add_catch_lev(),
    Debugged = get(self),
    Ref = make_ref(),
    case catch {Ref,expr(Expr,Bs0,Cm,false,Le,F)} of
	{Ref,Val} ->
	    dec_catch_lev(),
	    pop(Le),
	    Val;
	{'EXIT',{Debugged,Reason}} ->
	    pop(Le),
	    exit({Debugged,Reason});
	Other ->
	    dec_catch_lev(),
	    pop(Le),
	    {value,Other,Bs0}
    end;

%% Case statement
expr({'case',LineNo,E,Cs},Bs0,Cm,Lc,Le,F) ->
    {value,Val,Bs} = expr(E,Bs0,Cm,false,Le,F),
    case_clauses(Val,Cs,Bs,Cm,Lc,Le,LineNo,F);

%% If statement
expr({'if',LineNo,Cs},Bs,Cm,Lc,Le,F) ->
    if_clauses(Cs,Bs,Cm,Lc,Le,LineNo,F);

%% Matching expression
expr({match,LineNo,Lhs,Rhs0},Bs0,Cm,Lc,Le,F) ->
    {value,Rhs,Bs1} = expr(Rhs0,Bs0,Cm,false,Le,F),
    case match(Lhs,Rhs,Bs1) of
	{match,Bs} ->
	    {value,Rhs,Bs};
	nomatch ->
	    exit({{badmatch,Rhs},F},Cm,LineNo,Bs1)
    end;

%% Construct a fun
expr({make_fun,LineNo,Index,Uniq,Free0}, Bs, Cm, Lc, Le, F) ->
    Free = get_free_vars(Free0, Bs),
    Fun = erts_debug:make_fun({get(self),Cm,Index,Uniq,Free}),
    {value,Fun,Bs};

%% Local function call
expr({local_call,LineNo,Func,As0},Bs0,Cm,Lc,Le,F) ->
    {As,Bs} = eval_list(As0,Bs0,Cm,Le,LineNo,F),
    trace(call,{Le,LineNo,Cm,Func,As}),
    eval_function(Cm,Func,As,Bs,Cm,Lc,Le,F,LineNo,local);

%% Remote function call
expr({call_remote,LineNo,Mod,Func,As0},Bs0,Cm,Lc,Le,F) ->
    {As,Bs} = eval_list(As0,Bs0,Cm,Le,LineNo,F),
    trace(call,{Le,LineNo,Mod,Func,As}),
    eval_function(Mod,Func,As,Bs,Cm,Lc,Le,F,LineNo,extern);

%% Call to self/0 (optimization)
expr(self,Bs,Cm,Lc,Le,F) ->
    {value,get(self),Bs};

%% Call to "safe" BIF,ie a BIF that can safely be executed in Meta
expr({safe_bif,LineNo,Mod,Func,As0},Bs0,Cm,Lc,Le,F) ->
    {As,Bs} = eval_list(As0,Bs0,Cm,Le,LineNo,F),
    safe_bif(catch_bif(get_catch_lev()),Func,As,Bs,Cm,Lc,Le,LineNo,F);

%% Call to a BIF that must be evaluated in the correct process
expr({bif,LineNo,Mod,Func,As0},Bs0,Cm,Lc,Le,F) ->
    {As,Bs} = eval_list(As0,Bs0,Cm,Le,LineNo,F),
    Debugged = get(self),
    trace(bif,{Le,LineNo,Func,As}),
    Debugged ! {sys,self(),{catch_bif(get_catch_lev()),Mod,Func,As,
			    clean_mfa(F),false}},
    dbg_imeta:main_meta_loop(Debugged,Bs,Le,Lc,Cm,LineNo,F);

%% Call to a BIF that spawns a new process
expr({spawn_bif,LineNo,Mod,Func,As0},Bs0,Cm,Lc,Le,F) ->
    {As,Bs} = eval_list(As0,Bs0,Cm,Le,LineNo,F),
    Debugged = get(self),
    trace(bif,{Le,LineNo,Func,As}),
    Debugged ! {sys,self(),{catch_bif(get_catch_lev()),Mod,Func,As,
			       clean_mfa(F),follow_mod()}},
    dbg_imeta:main_meta_loop(Debugged,Bs,Le,Lc,Cm,LineNo,F);

%% Call to an operation
expr({op,LineNo,Op,As0},Bs0,Cm,Lc,Le,F) ->
    {As,Bs} = eval_list(As0,Bs0,Cm,Le,LineNo,F),
    case catch apply(erlang,Op,As) of
	{'EXIT',{Reason,Where}} ->
	    exit({Reason,F},Cm,LineNo,Bs);
	Value ->
	    {value,Value,Bs}
    end;

%% apply/2 (fun)
expr({apply_fun,LineNo,Fun0,As0},Bs0,Cm,Lc,Le,F) ->
    case expr(Fun0,Bs0,Cm,Lc,Le,F) of
	{value,Fun,Bs1} when function(Fun) ->
	    {As,Bs} = eval_list(As0,Bs1,Cm,Le,LineNo,F),
	    eval_function(unknown,Fun,As,Bs,Cm,Lc,Le,F,LineNo,extern);
	{value,{Mod,Func},Bs1} when atom(Mod),atom(Func) ->
	    {As,Bs} = eval_list(As0,Bs1,Cm,Le,LineNo,F),
	    eval_function(Mod,Func,As,Bs,Cm,Lc,Le,F,LineNo,extern);
	{value,BadFun,Bs1} ->
	    exit({{badfun,BadFun},F},Cm,LineNo,Bs1)
    end;

%% apply/3
expr({apply,LineNo,As0},Bs0,Cm,Lc,Le,F) ->
    {[Mod,Name,As],Bs} = eval_list(As0,Bs0,Cm,Le,LineNo,F),
    eval_function(Mod,Name,As,Bs,Cm,Lc,Le,F,LineNo,extern);
    
%% throw
expr({throw,LineNo,As0},Bs0,Cm,Lc,Le,F) ->
    {value,Term,Bs} = expr(As0,Bs0,Cm,Lc,Le,F),
    Debugged = get(self),
    trace(bif,{Le,LineNo,throw,[Term]}),
    case get_catch_lev() of
	0 -> % No surrounding catch interpreted
	    Debugged ! {sys,self(),
			   {catch_bif(get_catch_lev()),erlang,throw,[Term],
			    clean_mfa(F),false}},
	    dbg_imeta:main_meta_loop(Debugged,Bs,Le,Lc,Cm,LineNo,F);
	N ->
	    throw(Term)
    end;

%% Mod:module_info/0,1
expr({module_info_0,LineNo,Mod},Bs,Cm,Lc,Le,F) ->
    {value,[{compile,module_info(Mod,compile)},
	    {attributes,module_info(Mod,attributes)},
	    {imports,module_info(Mod,imports)},
	    {exports,module_info(Mod,exports)}],Bs};
expr({module_info_1,LineNo,Mod,[As0]},Bs0,Cm,Lc,Le,F) ->
    {value,What,Bs} = expr(As0,Bs0,Cm,Lc,Le,F),
    {value,module_info(Mod,What),Bs};

%% Receive statement
expr({'receive',LineNo,Cs},Bs0,Cm,Lc,Le,F) ->
    trace(receivex,{Le,false}),
    eval_receive(get(self),Cs,Bs0,Cm,Lc,Le,LineNo,F);

%% Receive..after statement
expr({'receive',LineNo,Cs,To,ToExprs},Bs0,Cm,Lc,Le,F) ->
    {value,ToVal,ToBs} = expr(To,Bs0,Cm,false,Le,F),
    trace(receivex,{Le,true}),
    check_timeoutvalue(ToVal,ToBs,To,Cm,F),
    {Stamp,_} = statistics(wall_clock),
    eval_receive(get(self),Cs,ToVal,ToExprs,ToBs,Bs0,Cm,Lc,Le,LineNo,0,Stamp,F);

%% Send (!)
expr({send,LineNo,T0,M0},Bs0,Cm,Lc,Le,F) ->
    {value,T,Bs1} = expr(T0,Bs0,Cm,false,Le,F),
    {value,M,Bs2} = expr(M0,Bs0,Cm,false,Le,F),
    Bs = merge_bindings(Bs2,Bs1,Cm,LineNo,F),
    eval_send(LineNo,T,M,Bs,Cm,F);

%% Binary
expr({bin,_,Fs},Bs0,Cm,Lc,Le,F) ->
    eval_bits:expr_grp(Fs,Bs0,
		       fun(E,B) -> expr(E,B,Cm,Lc,Le,F) end,
		       [],
		       false);

%% List comprehension
expr({lc,LineNo,E,Qs},Bs,Cm,Lc,Le,F) ->
    eval_lc(E,Qs,Bs,Cm,Lc,Le,F);

%% Brutal exit on unknown expressions/clauses/values/etc.
expr(E,_,_,_,_,_) ->
    exit({'NYI',E}).

%% eval_lc(Expr,[Qualifier],Bindings,Mod,LastCall,Level,Func) ->
%%	{value,Value,Bindings}.
%% This is evaluating list comprehensions "straight out of the book".
%% Copied from rv's implementation in erl_eval.
eval_lc(E,Qs,Bs,Cm,Lc,Le,F) ->
    {value,eval_lc1(E,Qs,Bs,Cm,Lc,Le,F),Bs}.
eval_lc1(E,[{generate,_,P,L0}|Qs],Bs0,Cm,Lc,Le,F) ->
    {value,L1,Bs1} = expr(L0,Bs0,Cm,false,Le,F),
    lists:flatmap(fun (V) ->
		    case match(P,V,[]) of
			{match,Bsn} ->
			    Bs2 = add_bindings(Bsn,Bs1),
			    eval_lc1(E,Qs,Bs2,Cm,Lc,Le,F);
			nomatch -> []
		    end end,L1);
eval_lc1(E,[{guard,Q}|Qs],Bs0,Cm,Lc,Le,F) ->
    case guard(Q,Bs0) of
	true -> eval_lc1(E,Qs,Bs0,Cm,Lc,Le,F);
	false -> []
    end;
eval_lc1(E,[Q|Qs],Bs0,Cm,Lc,Le,F) ->
    case expr(Q,Bs0,Cm,false,Le,F) of
	{value,true,Bs1} -> eval_lc1(E,Qs,Bs1,Cm,Lc,Le,F);
	{value,false,Bs1} -> [];
	Other -> exit({bad_filter,in_fnk(F)})
    end;
eval_lc1(E,[],Bs,Cm,Lc,Le,F) ->
    {value,V,_} = expr(E,Bs,Cm,false,Le,F),
    [V].

module_info(Mod,module) -> Mod;
module_info(Mod,compile) -> [];
module_info(Mod,attributes) ->
    {ok, Attr} = dbg_iserver:call(get(int), {lookup, Mod, attributes}),
    Attr;
module_info(Mod,imports) -> [];
module_info(Mod,exports) ->
    {ok, Exp} = dbg_iserver:call(get(int), {lookup, Mod, exports}),
    Exp;
module_info(Mod,functions) -> [].

get_free_vars([V|Vs],Bs) ->
    {value,Val} = binding(V,Bs),
    [Val|get_free_vars(Vs,Bs)];
get_free_vars([],Bs) -> [].

catch_apply(0) -> apply;
catch_apply(Lev) when integer(Lev) -> catch_apply.

%% Ask attach-process where to start new attach-process
follow_mod() ->
    case get(next_break) of
	break -> follow_mod1();
	_ -> false
    end.

follow_mod1() ->
    case get(attached) of
	[AttPid|_] -> {followed, AttPid};
	[] -> false
    end.

catch_bif(0) -> bif;
catch_bif(Lev) when integer(Lev) -> catch_bif.

safe_bif(Catch,Name,As,Bs,Cm,Lc,Le,LineNo,F) ->
    Ref = make_ref(),
    case catch {Ref,erts_debug:apply(erlang,Name,As,clean_mfa(F))} of
	{Ref,Value} when Lc==true ->
	    {value,Value,Bs};
	{Ref,Value} ->
	    trace(return,{Le,Value,Lc}),
	    {value,Value,Bs};
	{'EXIT',Reason} ->
	    case Catch of
		bif -> exit(Reason,Cm,LineNo,Bs);
		catch_bif -> exit(Reason)
	    end
    end.

eval_send(LineNo,To,Msg,Bs,Cm,F) ->
    case catch To ! Msg of
	Msg ->
	    trace(send,{To,Msg}),
	    {value,Msg,Bs};
	{'EXIT',{badarg,{T,M}}} ->  % If To is an non-existing name
	    exit({badarg,{T,M}},Cm,LineNo,Bs);
	{'EXIT',Reason} ->          % Reason is badarg... if send fails !!
	    exit({badarg,F},Cm,LineNo,Bs)
    end.

%% Start tracing of messages before fetching current messages in the queue
%% to make sure that no messages are lost. 
eval_receive(Debugged,Cs,Bs0,Cm,Lc,Le,LineNo,F) ->
    %% To avoid private message passing protocol between META
    %% and interpreted process.
    erlang:trace(Debugged,true,['receive']),
    {_,Msgs} = erlang:process_info(Debugged,messages),
    case receive_clauses(Cs,Bs0,Cm,Le,Msgs) of
	nomatch ->
	    dbg_iserver:cast(get(int), {set_status, self(), waiting, {}}),
	    dbg_icmd:tell_attached_if_break({wait_at,Cm,LineNo,Le}),
	    eval_receive1(Debugged,Cs,Bs0,Cm,Lc,Le,LineNo,F);
	{eval,B,Bs,Msg} ->
	    rec_mess(Debugged,Msg,Cm,LineNo,Bs0),
	    seq(B,Bs,Cm,Lc,Le,F)
    end.

eval_receive1(Debugged,Cs,Bs0,Cm,Lc,Le,LineNo,F) ->
    Msgs = do_receive(Debugged,Cm,Le,LineNo,Bs0),
    case receive_clauses(Cs,Bs0,Cm,Le,Msgs) of
	nomatch ->
	    eval_receive1(Debugged,Cs,Bs0,Cm,Lc,Le,LineNo,F);
	{eval,B,Bs,Msg} ->
	    rec_mess(Debugged,Msg,Cm,LineNo,Bs0),
	    dbg_iserver:cast(get(int), {set_status, self(), running, {}}),
	    dbg_icmd:tell_attached_if_break(running),
	    seq(B,Bs,Cm,Lc,Le,F)
    end.

check_timeoutvalue(ToVal,_,_,_,_) when integer(ToVal),ToVal>=0 -> true;
check_timeoutvalue(infinity,_,_,_,_) -> true;
check_timeoutvalue(ToVal,ToBs,To,Cm,F) ->
    LineNo = element(2,To),
    exit({timeout_value,F},Cm,LineNo,ToBs).

eval_receive(Debugged,Cs,0,ToExprs,ToBs,Bs0,Cm,Lc,Le,LineNo,0,Stamp,F) ->
    {_,Msgs} = erlang:process_info(Debugged,messages),% at the first call
    case receive_clauses(Cs,Bs0,Cm,Le,Msgs) of
	{eval,B,Bs,Msg} ->
	    rec_mess_no_trace(Debugged,Msg,Cm,LineNo,Bs0),
	    seq(B,Bs,Cm,Lc,Le,F);
	nomatch ->
	    trace(received,null),
	    seq(ToExprs,ToBs,Cm,Lc,Le,F)
    end;
eval_receive(Debugged,Cs,ToVal,ToExprs,ToBs,Bs0,Cm,Lc,Le,LineNo,0,Stamp,F)->
    erlang:trace(Debugged,true,['receive']),
    {_,Msgs} = erlang:process_info(Debugged,messages),
    case receive_clauses(Cs,Bs0,Cm,Le,Msgs) of
	nomatch ->
	    {Stamp1,Time1} = newtime(Stamp,ToVal),
	    dbg_iserver:cast(get(int), {set_status, self(), waiting, {}}),
	    dbg_icmd:tell_attached_if_break({wait_after_at,Cm,LineNo,Le}),
	    eval_receive(Debugged,Cs,Time1,ToExprs,ToBs,Bs0,Cm,Lc,Le,
			 LineNo,infinity,Stamp1,F);
	{eval,B,Bs,Msg} ->
	    rec_mess(Debugged,Msg,Cm,LineNo,Bs0),
	    seq(B,Bs,Cm,Lc,Le,F)
    end;
eval_receive(Debugged,Cs,ToVal,ToExprs,ToBs,Bs0,Cm,Lc,Le,LineNo,_,Stamp,F)->
    case do_receive(Debugged,ToVal,Stamp,Cm,Le,LineNo,Bs0) of
	timeout ->
	    trace(received,null),
	    rec_mess(Debugged),
	    dbg_iserver:cast(get(int), {set_status, self(), running, {}}),
	    dbg_icmd:tell_attached_if_break(running),
	    seq(ToExprs,ToBs,Cm,Lc,Le,F);
	Msgs ->
	    case receive_clauses(Cs,Bs0,Cm,Le,Msgs) of
		nomatch ->
		    {Stamp1,Time1} = newtime(Stamp,ToVal),
		    eval_receive(Debugged,Cs,Time1,ToExprs,ToBs,Bs0,Cm,Lc,Le,LineNo,
				 infinity,Stamp1,F);
		{eval,B,Bs,Msg} ->
		    rec_mess(Debugged,Msg,Cm,LineNo,Bs0),
		    dbg_iserver:cast(get(int),
				     {set_status, self(), running, {}}),
		    dbg_icmd:tell_attached_if_break(running),
		    seq(B,Bs,Cm,Lc,Le,F)
	    end
    end.

do_receive(Debugged,Cm,Le,LineNo,Bs) ->
    receive
	{trace,Debugged,'receive',Msg} ->
	    [Msg];

	{'EXIT',Debugged,Reason} ->
	    exit(Debugged,Reason,Cm,LineNo,Bs);
	{'EXIT',Pid,Reason} ->
	    exit({int,Reason});

	Msg ->
	    dbg_icmd:handle_msg(Msg, {wait, Bs, Le, Cm, LineNo}),
	    do_receive(Debugged,Cm,Le,LineNo,Bs)
    end.

do_receive(Debugged,Time,Stamp,Cm,Le,LineNo,Bs) ->
    receive
	{trace,Debugged,'receive',Msg} ->
	    [Msg];
	timeout ->
	    timeout;

	{'EXIT',Debugged,Reason} ->
	    exit(Debugged,Reason,Cm,LineNo,Bs);
	{'EXIT',Pid,Reason} ->
	    exit({int,Reason});

	Msg ->
	    dbg_icmd:handle_msg(Msg, {wait_after, Bs, Le, Cm, LineNo}),
	    {Stamp1,Time1} = newtime(Stamp,Time),
	    do_receive(Debugged,Time1,Stamp1,Cm,Le,LineNo,Bs)
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

rec_mess(Debugged,Msg,Cm,LineNo,Bs) ->
    erlang:trace(Debugged,false,['receive']),
    flush_traces(Debugged),
    Debugged ! {sys,self(),{'receive',Msg}},
    rec_ack(Debugged,Cm,LineNo,Bs).

rec_mess(Debugged) ->
    erlang:trace(Debugged,false,['receive']),
    flush_traces(Debugged).

rec_mess_no_trace(Debugged,Msg,Cm,LineNo,Bs) ->
    Debugged ! {sys,self(),{'receive',Msg}},
    rec_ack(Debugged,Cm,LineNo,Bs).

rec_ack(Debugged,Cm,LineNo,Bs) ->
    receive
	{Debugged,rec_acked} ->
	    true;
	{'EXIT',Debugged,Reason} ->
	    exit(Debugged,Reason,Cm,LineNo,Bs)
    end.

flush_traces(Debugged) ->
    receive
	{trace,Debugged,'receive',_} ->
	    flush_traces(Debugged)
    after 0 ->
	    true
    end.

%% eval_list(ExpressionList,Bindings,CurrentMod,Level,LineNo,Function)
%%  Evaluate a list of expressions "in parallel" at the same level.
eval_list(Es,Bs,Cm,Le,LineNo,F) ->
    eval_list(Es,[],Bs,Bs,Cm,Le,LineNo,F).
eval_list([E|Es],Vs,BsOrig,Bs0,Cm,Le,LineNo,F) ->
    {value,V,Bs1} = expr(E,BsOrig,Cm,false,Le,F),
    eval_list(Es,[V|Vs],BsOrig,merge_bindings(Bs1,Bs0,Cm,LineNo,F),
	      Cm,Le,LineNo,F);
eval_list([],Vs,_,Bs,_,_,_,_) ->
    {lists:reverse(Vs,[]),Bs}.

%% if_clauses(Clauses,Bindings,CurrentMod,LastCall,LineNo,Function)
if_clauses([{clause,_,[],G,B}|Cs],Bs,Cm,Lc,Le,LineNo,F) ->
    case guard(G,Bs) of
	true ->
	    seq(B,Bs,Cm,Lc,Le,F);
	false ->
	    if_clauses(Cs,Bs,Cm,Lc,Le,LineNo,F)
    end;
if_clauses([],Bs,Cm,Lc,Le,LineNo,F) ->
    exit({if_clause,F},Cm,LineNo,Bs).

%% case_clauses(Value,Clauses,Bindings,CurrentMod,LastCall,LineNo,Function)
case_clauses(Val,[{clause,_,[P],G,B}|Cs],Bs0,Cm,Lc,Le,LineNo,F) ->
    case match(P,Val,Bs0) of
	{match,Bs} ->
	    case guard(G,Bs) of
		true ->
		    seq(B,Bs,Cm,Lc,Le,F);
		false ->
		    case_clauses(Val,Cs,Bs0,Cm,Lc,Le,LineNo,F)
	    end;
	nomatch ->
	    case_clauses(Val,Cs,Bs0,Cm,Lc,Le,LineNo,F)
    end;
case_clauses(Val,[],Bs,Cm,Lc,Le,LineNo,F) ->
    exit({{case_clause,Val},F},Cm,LineNo,Bs).

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
	    case guard(G,Bs) of
		true ->
		    trace(received,Msg),
		    {eval,B,Bs};
		false ->
		    rec_clauses(Cs,Bs0,Cm,Le,Msg,Clause+1)
	    end;
	nomatch ->
	    rec_clauses(Cs,Bs0,Cm,Le,Msg,Clause+1)
    end;
rec_clauses([],_,_,_,_,_) ->
    nomatch.

%% guard(GuardTests,Bindings)
%%  Evaluate a list of guards.
guard([],Bs) -> true;
guard(Gs,Bs) -> or_guard(Gs,Bs).
    
or_guard([G|Gs],Bs) ->
    %% Short-circuit OR.
    case and_guard(G,Bs) of
	true -> true;
	false -> or_guard(Gs,Bs)
    end;
or_guard([],Bs) -> false.

and_guard([G|Gs],Bs) ->
    %% Short-circuit AND.
    case catch guard_expr(G,Bs) of
	{value,true} -> and_guard(Gs,Bs);
	Other -> false
    end;
and_guard([],_) -> true.

guard_exprs([A0|As0],Bs) ->
    {value,A} = guard_expr(A0,Bs),
    {values,As} = guard_exprs(As0,Bs),
    {values,[A|As]};
guard_exprs([],Bs) ->
    {values,[]}.

guard_expr(self,Bs) ->
    {value,get(self)};
guard_expr({safe_bif,LineNo,Mod,Func,As0},Bs) ->
    {values,As} = guard_exprs(As0,Bs),
    {value,apply(Mod,Func,As)};
guard_expr({var,LineNo,V},Bs) ->
    {value,_} = binding(V,Bs);
guard_expr({value,LineNo,Val},Bs) ->
    {value,Val};
guard_expr({cons,LineNo,H0,T0},Bs) ->
    {value,H} = guard_expr(H0,Bs),
    {value,T} = guard_expr(T0,Bs),
    {value,[H|T]};
guard_expr({tuple,LineNo,Es0},Bs) ->
    {values,Es} = guard_exprs(Es0,Bs),
    {value,list_to_tuple(Es)};
guard_expr({bin,LineNo,Flds},Bs) ->
    {value,V,_Bs} = eval_bits:expr_grp(Flds,Bs,
				       fun(E,B) ->
					       {value,V} = guard_expr(E,B),
					       {value,V,B}
				       end,[],false),
    {value,V}.

%% match(Pattern,Term,Bs) -> {match,Bs} | nomatch
match(Pat,Term,Bs) ->
    catch match1(Pat,Term,Bs).
match1({value,LineNo,V},V,Bs) ->
    {match,Bs};
match1({var,_,'_'},Term,Bs) -> % Anonymous variable matches
    {match,add_anon(Term,Bs)};   % everything,save it anyway
match1({var,_,Name},Term,Bs) ->
    case binding(Name,Bs) of
	{value,Term} ->
	    {match,Bs};
	{value,V} ->
	    throw(nomatch);
	unbound ->
	    {match,[{Name,Term}|Bs]} % Add the new binding
    end;
match1({match,LineNo,Pat1,Pat2},Term,Bs0) ->
    {match,Bs1} = match1(Pat1,Term,Bs0),
    match1(Pat2,Term,Bs1);
match1({cons,_,H,T},[H1|T1],Bs0) ->
    {match,Bs} = match1(H,H1,Bs0),
    match1(T,T1,Bs);
match1({tuple,_,Elts},Tuple,Bs) when tuple(Tuple),length(Elts)=:=size(Tuple) ->
    match_tuple(Elts,Tuple,1,Bs);
match1({bin,_,Fs},B,Bs0) when binary(B) ->
    eval_bits:match_bits(Fs,B,Bs0,
			 fun(L,R,Bs) -> match1(L,R,Bs) end,
			 fun(E,Bs) -> expr(E,Bs,foo,foo,foo,foo) end,
			 false);
match1(_,_,_) ->
    throw(nomatch).

match_tuple([E|Es],Tuple,I,Bs0) ->
    {match,Bs} = match1(E,element(I,Tuple),Bs0),
    match_tuple(Es,Tuple,I+1,Bs);
match_tuple([],_,_,Bs) ->
    {match,Bs}.

head_match([Par|Pars],[Arg|Args],Bs0) ->
    case match(Par,Arg,Bs0) of
	{match,Bs} -> head_match(Pars,Args,Bs);
	nomatch -> nomatch
    end;
head_match([],[],Bs) -> {match,Bs}.

rec_match([Par],Msg,Bs0) ->
    match(Par,Msg,Bs0).

binding(Name,[{Name,Val}|_]) ->
    {value,Val};
binding(Name,[_,{Name,Val}|_]) ->
    {value,Val};
binding(Name,[_,_,{Name,Val}|_]) ->
    {value,Val};
binding(Name,[_,_,_,{Name,Val}|_]) ->
    {value,Val};
binding(Name,[_,_,_,_,{Name,Val}|_]) ->
    {value,Val};
binding(Name,[_,_,_,_,_,{Name,Val}|_]) ->
    {value,Val};
binding(Name,[_,_,_,_,_,_|Bs]) ->
    binding(Name,Bs);
binding(Name,[_,_,_,_,_|Bs]) ->
    binding(Name,Bs);
binding(Name,[_,_,_,_|Bs]) ->
    binding(Name,Bs);
binding(Name,[_,_,_|Bs]) ->
    binding(Name,Bs);
binding(Name,[_,_|Bs]) ->
    binding(Name,Bs);
binding(Name,[_|Bs]) ->
    binding(Name,Bs);
binding(_,[]) ->
    unbound.

add_anon(Val,[{'_',_}|Bs]) ->
    [{'_',Val}|Bs];
add_anon(Val,[B1,{'_',_}|Bs]) ->
    [B1,{'_',Val}|Bs];
add_anon(Val,[B1,B2,{'_',_}|Bs]) ->
    [B1,B2,{'_',Val}|Bs];
add_anon(Val,[B1,B2,B3,{'_',_}|Bs]) ->
    [B1,B2,B3,{'_',Val}|Bs];
add_anon(Val,[B1,B2,B3,B4,{'_',_}|Bs]) ->
    [B1,B2,B3,B4,{'_',Val}|Bs];
add_anon(Val,[B1,B2,B3,B4,B5,{'_',_}|Bs]) ->
    [B1,B2,B3,B4,B5,{'_',Val}|Bs];
add_anon(Val,[B1,B2,B3,B4,B5,B6|Bs]) ->
    [B1,B2,B3,B4,B5,B6|add_anon(Val,Bs)];
add_anon(Val,[B1,B2,B3,B4,B5|Bs]) ->
    [B1,B2,B3,B4,B5|add_anon(Val,Bs)];
add_anon(Val,[B1,B2,B3,B4|Bs]) ->
    [B1,B2,B3,B4|add_anon(Val,Bs)];
add_anon(Val,[B1,B2,B3|Bs]) ->
    [B1,B2,B3|add_anon(Val,Bs)];
add_anon(Val,[B1,B2|Bs]) ->
    [B1,B2|add_anon(Val,Bs)];
add_anon(Val,[B1|Bs]) ->
    [B1|add_anon(Val,Bs)];
add_anon(Val,[]) ->
    [{'_',Val}].

%% merge_bindings(Bindings1,Bindings2,Cm,LineNo,Function)
%% Merge bindings detecting bad matches. 
%% Special case '_',save the new one !!!
%% Bindings1 is the newest bindings.
merge_bindings(Bs,Bs,_,_,_) ->
    Bs; % Identical bindings
merge_bindings([{Name,V}|B1s],B2s,Cm,LineNo,F) ->
    case binding(Name,B2s) of
	{value,V} -> % Already there,and the SAME
	    merge_bindings(B1s,B2s,Cm,LineNo,F);
	{value,V1} when Name == '_' ->		% but anonym different
	    B2s1 = lists:keydelete('_',1,B2s),
	    [{Name,V}|merge_bindings(B1s,B2s1,Cm,LineNo,F)];
	{value,V1} ->				% but different
	    exit({{badmatch,V},F},Cm,LineNo,B2s);
	unbound ->				% Not there,add it
	    [{Name,V}|merge_bindings(B1s,B2s,Cm,LineNo,F)]
    end;
merge_bindings([],B2s,_,_,_) ->
    B2s.

%% add_bindings(Bindings1,Bindings2)
%% Add Bindings1 to Bindings2. Bindings in
%% Bindings1 hides bindings in Bindings2.
%% Used in list comprehensions (and funs).
add_bindings([{Name,V}|Bs],ToBs0) ->
    ToBs = add_binding(Name,V,ToBs0),
    add_bindings(Bs,ToBs);
add_bindings([],ToBs) ->
    ToBs.

add_binding(N,Val,[{N,_}|Bs]) ->
    [{N,Val}|Bs];
add_binding(N,Val,[B1,{N,_}|Bs]) ->
    [B1,{N,Val}|Bs];
add_binding(N,Val,[B1,B2,{N,_}|Bs]) ->
    [B1,B2,{N,Val}|Bs];
add_binding(N,Val,[B1,B2,B3,{N,_}|Bs]) ->
    [B1,B2,B3,{N,Val}|Bs];
add_binding(N,Val,[B1,B2,B3,B4,{N,_}|Bs]) ->
    [B1,B2,B3,B4,{N,Val}|Bs];
add_binding(N,Val,[B1,B2,B3,B4,B5,{N,_}|Bs]) ->
    [B1,B2,B3,B4,B5,{N,Val}|Bs];
add_binding(N,Val,[B1,B2,B3,B4,B5,B6|Bs]) ->
    [B1,B2,B3,B4,B5,B6|add_binding(N,Val,Bs)];
add_binding(N,Val,[B1,B2,B3,B4,B5|Bs]) ->
    [B1,B2,B3,B4,B5|add_binding(N,Val,Bs)];
add_binding(N,Val,[B1,B2,B3,B4|Bs]) ->
    [B1,B2,B3,B4|add_binding(N,Val,Bs)];
add_binding(N,Val,[B1,B2,B3|Bs]) ->
    [B1,B2,B3|add_binding(N,Val,Bs)];
add_binding(N,Val,[B1,B2|Bs]) ->
    [B1,B2|add_binding(N,Val,Bs)];
add_binding(N,Val,[B1|Bs]) ->
    [B1|add_binding(N,Val,Bs)];
add_binding(N,Val,[]) ->
    [{N,Val}].

add_catch_lev() ->
    [Cur|Levs] = get(catch_lev),
    put(catch_lev, [Cur+1|Levs]).
dec_catch_lev() ->
    [Cur|Levs] = get(catch_lev),
    put(catch_lev, [Cur-1|Levs]).
get_catch_lev() ->
    [Lev|_] = get(catch_lev),
    Lev.

init_catch_lev() ->
    put(catch_lev, [0 | get(catch_lev)]).
exit_catch_lev() ->
    put(catch_lev, tl(get(catch_lev))).
