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
%% Purpose : Erlang meta evaluator

-module(dbg_imeta).

-export([eval/3,add_catch_lev/0,dec_catch_lev/0,get_catch_lev/0,
	 lambda/2,function/4,exit_info/4,main_meta_loop/7]).

-import(error_logger, [error_msg/2]).

%% eval(Mod, Func, Args) -> MetaPid
%%  Entry point from process being debugged.
%%
%%  Immediately returns the pid for the meta process.
%%  The evaluated value will later be sent as a message to
%%  process that called this function.

eval(Mod, Func, Args) ->
    Interpreter = whereis(interpret),
    Interpreter ! {self(),{interpreted,self()}},
    Meta = receive
	       {Interpreter,{interpreted,Res}} -> Res
	   end,
    Debugged = self(),
    case Meta of
	false ->
	    spawn_link(fun() -> int(Debugged, Mod, Func, Args) end);
	Pid ->
	    Pid ! {error_handler,Debugged,{eval,{Mod,Func,Args}}},
	    Pid
    end.

int(Debugged, M, F, As) ->
    %% Entry point for first-time initialization of meta process.
    process_flag(trap_exit, true),
    State = dbg_iserver_api:int_proc(Debugged, M, F, As),
    put(self, Debugged),
    put(next_break, State),
    put(error, none),
    put(attached, []),
    put(user_cmd, []),
    dbg_icmd:init_breaks(),
    put(catch_lev, []),
    Trace = trace_p(),
    put(trace_f, Trace),
    put(trace, false),
    put(stack_trace, stack_p()),
    put(stack, []),

    eval_mfa(Debugged, M, F, As, 1),
    dbg_iserver_api:set_state(idle),
    tell_att_if_break(idle),
    main_meta_loop(Debugged, [], 1, false, extern, -1, extern).

eval_mfa(Debugged, M, F, As, Le) ->
    case get(trace) of
	true ->
	    Text = io_lib:format("++ (1) ~w:~w(~s)\n",
				 [M,F,dbg_ieval:format_args(As)]),
	    dbg_ieval:trace_out(Text);
	false -> ok
    end,
    new_catch_lev(),
    case dbg_ieval:eval_function(M, F, As, Le) of
	{value,Val,Bs} ->
	    Debugged ! {sys,self(),{ready,Val}},
	    del_catch_lev();

	{'EXIT',{Debugged,Reason}} ->
	    del_catch_lev(),
	    dbg_ieval:pop(Le),
	    if
		Le > 1 -> exit({Debugged,Reason});
		true -> do_real_exit(Reason)
	    end;

	{'EXIT',{confirmed,Reason}} ->
	    Debugged ! {sys,self(),{exit,Reason}},
	    if
		Le > 1 ->
		    receive
			{sys,Debugged,{exited_nocatch,Reason}} ->
			    Debugged ! {sys,self(),{exit,Reason}}
		    end;
		true -> ok
	    end,
	    del_catch_lev(),
	    dbg_ieval:pop(Le);

	{'EXIT',Reason} ->
	    Debugged ! {sys,self(),{exit,Reason}},
	    del_catch_lev(),
	    dbg_ieval:pop(Le);

	%% FIXME: WHy does this clause exist?
	Thrown ->
	    del_catch_lev(),
	    dbg_ieval:pop(Le)
    end.

trace_p() ->
    case dbg_idb:lookup(trace) of
	{ok,true} ->
	    true;
	_ ->
	    false
    end.

stack_p() ->
    case dbg_idb:lookup(stack_trace) of
	{ok,true} -> all;
	{ok,Flag} -> Flag;
	_ -> false
    end.

%% main_meta_loop/7 - Main receive loop for the meta process.
%%
%% Returns: {value,Value,Bindings}

main_meta_loop(Debugged, Bs, Le, Lc, Cm, Line, F) when integer(Le) ->
    receive
	%% The following messages can only be received
	%% when we (the meta process) is waiting for the debugged
	%% process (Debugged) to evaluate non-interpreted code
	%% or a BIF. Le > 1.

	{sys,Debugged,{apply_result,Val}} when Lc == false ->
	    dbg_ieval:trace_fnk_ret(Le, Val, Lc),
	    {value,Val,Bs};
	{sys,Debugged,{apply_result,Val}} ->
	    {value,Val,Bs};
	{sys,Debugged,{eval_result,Val,NewBs}} ->
	    {value,Val,NewBs};
	{sys,Debugged,{thrown,Value}} ->
	    throw(Value);
	{sys,Debugged,{thrown_nocatch,Value}} ->
	    throw(Value, Cm, Line, Bs);

	%% The following messages can be received any time.
	%% If Le == 1, the meta process is at the top level.
	%% If Le > 1, the meta process has been entered
	%% more than once from the error_handler module.

	{sys,Debugged,{exited_nocatch,Reason}} when Le == 1->
	    Debugged ! {sys,self(),{exit,Reason}},  % Yes, you shall die !!
	    main_meta_loop(Debugged, Bs, Le, Lc, Cm, Line, F);
	{sys,Debugged,{exited_nocatch,Reason}} ->
	    confirmed_exit(Reason, Cm, Line, Bs);

	%% Re-entry to the META process at top level.
	{error_handler,Debugged,{eval,{Mod,Func,As}}} when Le == 1 ->
	    dbg_iserver_api:set_state(running),
	    tell_att_if_break(running),

	    %% Tell the attach window to update source code.
	    dbg_icmd:tell_attached({self(),re_entry, Mod, Func}),

	    eval_mfa(Debugged, Mod, Func, As, 1),
	    dbg_iserver_api:set_state(idle),
	    tell_att_if_break(idle),
	    main_meta_loop(Debugged, Bs, Le, Lc, Cm, Line, F);
	{error_handler,Debugged,{eval,{Mod,Func,Args}}} ->
	    eval_mfa(Debugged, Mod, Func, Args, Le),
	    main_meta_loop(Debugged, Bs, Le, Lc, Cm, Line, F);

	{attach,Debugged,AttPid} when Le == 1 ->
	    dbg_icmd:attach(AttPid),
	    main_meta_loop(Debugged, Bs, Le, Lc, Cm, Line, F);
	{attach,Debugged,AttPid} ->
	    dbg_icmd:attach(AttPid,Cm,Line),
	    AttPid ! {self(),func_at,Cm,Line,Le},
	    main_meta_loop(Debugged, Bs, Le, Lc, Cm, Line, F);

	{break_msg,Type,Data} ->
	    dbg_icmd:break_msg(Type,Data),
	    main_meta_loop(Debugged, Bs, Le, Lc, Cm, Line, F);

	%% The code in Module is no longer interpreted.
	{old_code, Module} when Le == 1 ->
	    erase(cache),
	    erase([Module|db]),
	    main_meta_loop(Debugged, Bs, Le, Lc, Cm, Line, F);
	{old_code, Module} ->
	    case dbg_ieval:in_use_p(Module, Cm) of
		true ->
		    %% A call to Module is on the stack (or might be),
		    %% so we must terminate.
		    exit(get(self), kill),
		    dbg_ieval:exit(old_code, Cm, Line, Bs);
		false ->
		    erase([Module|db]),
		    erase(cache),
		    main_meta_loop(Debugged, Bs, Le, Lc, Cm, Line, F)
	    end;


	%%FIXME: Handle new code
	{new_code,Module} ->				
	    io:format("~p:~p: NYI: Handling of {new_code,~p}\n",
		      [?MODULE,?LINE,Module]),
	    erase(cache), 
	    erase([Module|db]),
	    main_meta_loop(Debugged, Bs, Le, Lc, Cm, Line, F);
	
	%% Signal received from dying interpreted process
	%% (due to exit in non-interpreted code).
	{'EXIT',Debugged,Reason} when Le == 1 ->
	    do_real_exit(Reason);
	{'EXIT',Debugged,Reason} ->
	    dbg_ieval:exit(Debugged, Reason, Cm, Line, Bs);

	%% Some other (?) linked process terminated. FIXME: Describe which
	{'EXIT',Pid,Reason} ->
	    case dbg_icmd:attached_p(Pid) of
		true ->
		    dbg_icmd:detach(Pid),
		    main_meta_loop(Debugged, Bs, Le, Lc, Cm, Line, F);
		false ->
		    if
			Le == 1 ->
			    do_real_exit(Reason);
			true ->
			    dbg_ieval:exit(Reason,Cm,Line,Bs)
		    end
	    end;

	{cmd,Cmd,Args} ->
	    case dbg_icmd:command(Cmd, Args, Bs) of
		{skip,_}=Skip ->
		    Skip;
		{run,_}=Run ->
		    Run;
		NewBs ->
		    main_meta_loop(Debugged, NewBs, Le, Lc, Cm, Line, F)
	    end
    end.

confirmed_exit(Reason, Cm, Line, Bs) ->
    put_error(Reason, Cm, Line, Bs),
    exit({confirmed,Reason}).

throw(Value,Cm,Line,Bs) ->
    do_put_error(nocatch,Cm,Line,Bs),
    throw(Value).

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
    BinStack = term_to_binary(dbg_ieval:get_stack()),
    put(error, {Reason,{Cm,Line},Bs,BinStack}),
    ok.

do_real_exit(Reason) ->
    case get(error) of
	{Reason,Where,Bs,Stack} ->
	    exit({self(),Reason,Where,Bs,Stack,att_p()});
	_ ->
	    exit({self(),Reason,att_p()})
    end.

%% -------------------------------------------------------
%% Return true if we want to be auto attached.
%% -------------------------------------------------------

att_p() ->
    case get(attached) of
	[] -> true;
	_  -> false
    end.

%% -------------------------------------------------------
%% A mini-meta process for a terminated process.
%% This process is started when a terminated process is
%% attached.
%% -------------------------------------------------------

exit_info(AttPid,OrigPid,Reason,{{Mod,Line},Bs,Stack}) ->
    process_flag(trap_exit,true),
    put(attached,[AttPid]),
    put(self,OrigPid),
    S = binary_to_term(Stack),
    put(stack,S),
    SP = sp(S),
    AttPid ! {self(),exit_at,Mod,Line,Reason,SP+1},
    attach(AttPid,OrigPid),
    dbg_icmd:init_breaks(),
    exit_loop(OrigPid,Reason,Mod,Line,Bs);
exit_info(AttPid,OrigPid,Reason,_) ->
    process_flag(trap_exit,true),
    put(attached,[AttPid]),
    put(self,OrigPid),
    put(stack,[]),
    AttPid ! {self(),exit_at,no,no,Reason,1},
    attach(AttPid,OrigPid),
    dbg_icmd:init_breaks(),
    exit_loop(OrigPid,Reason,no,no,[]).

attach(AttPid,OrigPid) when node(OrigPid) == node() ->
    int:attached(AttPid);
attach(AttPid,OrigPid) ->
    rpc:call(node(OrigPid),int,attached,[AttPid]).

sp([]) ->
    0;
sp(S) ->
    element(1,hd(S)).

exit_loop(OrigPid,Reason,Mod,Line,Bs) ->
    receive
	Cmd when tuple(Cmd), element(1,Cmd) == cmd ->
	    dbg_icmd:exit_cmd(Cmd,Bs),
	    exit_loop(OrigPid,Reason,Mod,Line,Bs);
	{break_msg,Type,Data} ->
	    dbg_icmd:break_msg(Type,Data),
	    exit_loop(OrigPid,Reason,Mod,Line,Bs);
	{'EXIT',_,Why} ->
	    exit(Why);
	_ ->
	    exit_loop(OrigPid,Reason,Mod,Line,Bs)
    end.
    
get_catch_lev() ->
    [Lev|_] = get(catch_lev),
    Lev.

new_catch_lev() ->
    put(catch_lev, [0|get(catch_lev)]).

del_catch_lev() ->
    [_|Lev] = get(catch_lev),
    put(catch_lev, Lev).

add_catch_lev() ->
    [Cur|Levs] = get(catch_lev),
    put(catch_lev, [Cur+1|Levs]).
	    
dec_catch_lev() ->
    [Cur|Levs] = get(catch_lev),
    put(catch_lev, [Cur-1|Levs]).

%% Look up the clauses for a fun.
%%
%% Returns: {Clauses,Mod,Name,Args} | not_interpreted | undef | badfun | badarity

lambda(Fun, As0) when function(Fun) ->
    {module,Mod} = erlang:fun_info(Fun, module),
    case fun_call_clauses(Mod, Fun) of
	{Name,Arity,Cs} ->
	    {env,Env} = erlang:fun_info(Fun, env),
	    case As0 ++ Env of
		As when length(As) =:= Arity ->
		    {Cs,Mod,Name,As};
		As ->
		    badarity
	    end;
	Error when atom(Error) ->
	    Error
    end;
lambda(Fun, As) -> badfun.

fun_call_clauses(Mod, Fun) ->
    {new_index,Index} = erlang:fun_info(Fun, new_index),
    {new_uniq,Uniq} = erlang:fun_info(Fun, new_uniq),
    case fun_call_clauses_1(Mod, {'fun',Mod,Index,Uniq}) of
	{_,_,_}=Data -> Data;
	Error when atom(Error) ->
%%	    exit({no_new_index,Index,Uniq}),
	    {index,OldIndex} = erlang:fun_info(Fun, index),
	    {uniq,OldUniq} = erlang:fun_info(Fun, uniq),
	    fun_call_clauses_1(Mod, {'fun',Mod,OldIndex,OldUniq})
    end.

fun_call_clauses_1(Mod, Key) ->
    case cached(Key) of
	false ->
	    case db_ref(Mod) of
		not_found -> not_interpreted;
		Db ->
		    case dbg_idb:lookup(Db, Key) of
			{ok,Data} ->
			    cache(Key, Data),
			    Data;
			Error ->
			    case dbg_idb:lookup(Db, module) of
				{ok,_} -> undef;
				Error -> not_interpreted
			    end
		    end
	    end;
	FunData when tuple(FunData) ->
	    FunData
    end.

%% Look up the clauses for a local or external call.
%%
%% Returns: Clauses | not_interpreted | undef

function(Mod, Name, Args, local) ->
    Arity = length(Args),
    Key = {Mod,Name,Arity},
    case cached(Key) of
	false ->
	    Db = db_ref(Mod),
	    case dbg_idb:match_object(Db, {{Mod,Name,Arity,'_'},'_'}) of
		[{{Mod,Name,Arity,Exp},Clauses}] ->
		    cache(Key, {Exp,Clauses}),
		    Clauses;
		Other ->
		    undef
	    end;
	{_Exp,Cs} ->
	    Cs
    end;
function(Mod, Name, Args, extern) ->
    Arity = length(Args),
    Key = {Mod,Name,Arity},
    case cached(Key) of
	false ->
	    case db_ref(Mod) of
		not_found -> not_interpreted;
		Db ->
		    case dbg_idb:lookup(Db, {Mod,Name,Arity,true}) of
			{ok,Data} ->
			    cache(Key, {true,Data}),
			    Data;
			Error ->
			    case dbg_idb:lookup(Db, module) of
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
	    case dbg_idb:mod_db(Mod, get(self)) of
		not_found ->
		    not_found;
		DbRef ->
		    put([Mod|db], DbRef),
		    DbRef
	    end;
	DbRef ->
	    DbRef
    end.


%% cache/2 -- Stores a the clauses for a function in a small cache.
%%
%% Note: The cache is stored in the process dictionary.
%%
%% Returns: nothing important.
%%

cache(Key, Data) ->
    case get(cache) of
	undefined ->
	    put(cache, [{Key,Data}]);
	Cached when length(Cached) < 5 ->
	    put(cache, [{Key,Data}|Cached]);
	[C1,C2,C3,C4|_] ->
	    put(cache, [{Key,Data},C1,C2,C3,C4])
    end.
	    

%% cached/1 -- Retrieve an item from the cache.
%%
%% The cache is stored in the process dictionary.
%% 
%% Returns:
%% Data			When the function is cached.
%% false		Otherwise.
%% 

cached(Key) ->
    case get(cache) of
	Cache when list(Cache) ->
	    case lists:keysearch(Key, 1, Cache)  of
		{value,{Key,Data}} -> Data;
		false -> false
	    end;
	_ ->
	    false
    end.

tell_att_if_break(Info) ->
    case get(next_break) of
	break ->
	    dbg_icmd:tell_attached({self(),Info});
	_     ->
	    true
    end.
