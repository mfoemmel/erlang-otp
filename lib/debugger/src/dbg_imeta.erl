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

%% -- Exported user functions.

-export([start/3,eval/4,int/2,int/3,
	 add_catch_lev/0,dec_catch_lev/0,get_catch_lev/0,
	 lambda/2,function/4,
	 exit_info/4]).

-import(error_logger,[error_msg/2]).

start(Mod,Func,Args) ->
    dbg_imsg:start({interpret,call,Mod,Func,Args}).

eval(Meta,Mod,Func,Args) ->
    Meta ! {error_handler,self(),{eval,{Mod,Func,Args}}},
    dbg_imsg:msg(Meta).

int(Msg_handler,{M,F,As},Le) ->
    case get(trace) of
	true ->
	    dbg_ieval:trace_out(io_lib:format('++ (~w) ~w:~w(~s)~n',
					     [Le,M,F,
					      dbg_ieval:format_args(As)]));
	_ ->
	    true
    end,
    new_catch_lev(),
    case catch dbg_ieval:eval_function(M, F, As, [],
				 M, false, Le, extern, -1, extern) of
	{value,Val,_} ->
	    Msg_handler ! {sys,self(),{ready,Val}},
	    del_catch_lev(),
	    ready;
	{'EXIT',{Msg_handler,Reason}} ->
	    del_catch_lev(),
	    dbg_ieval:pop(Le),
	    exit({Msg_handler,Reason});
	{'EXIT',{confirmed,Reason}} ->
	    Msg_handler ! {sys,self(),{exit,Reason}},
	    del_catch_lev(),
	    dbg_ieval:pop(Le),
	    ready;
	{'EXIT',Reason} ->
	    Msg_handler ! {sys,self(),{exit,Reason}},
	    sync_exit(Msg_handler),
	    del_catch_lev(),
	    dbg_ieval:pop(Le),
	    ready;
	Throwed ->
	    del_catch_lev(),
	    dbg_ieval:pop(Le),
	    ready
    end.

sync_exit(Msg_handler) ->
    receive
	{sys,Msg_handler,{exited_nocatch,Reason}} ->
	    Msg_handler ! {sys,self(),{exit,Reason}}  % Yes, you shall die !!
    end.

%% Does not return a value per se.
%% The result is returned by sending a message to 
%% the interpreted process (Msg_handler).
%%
int(Msg_handler,{interpret,call,M,F,As}) ->   %% Init interpretation
    process_flag(trap_exit,true),
    State = dbg_iserver_api:int_proc(Msg_handler, M, F, As),
    put(self,Msg_handler),
    put(state,State),
    put(error,none),
    put(attached,[]),
    put(user_cmd,[]),
    dbg_icmd:init_breaks(),
    create_catch_lev(),
    Trace = trace_p(),
    put(trace_f,Trace),
    put(trace,false),   %% Not initial attached 
    put(stack_trace,stack_p()),
    put(fnk_no,0),
    case catch dbg_ieval:eval_function(M, F, As, [],
				 M, false, 1, extern, -1, extern) of

	%% Finally, return the value to the caller
	{value,Val,_} ->
	    Msg_handler ! {sys,self(),{ready,Val}},
	    dbg_iserver_api:set_state(idle),
	    tell_att_if_break(idle),
	    wait_int(Msg_handler);

	%% Handles EXITs in the interpreted process caused by the environment.
	{'EXIT',{Msg_handler,Reason}} ->
	    dbg_ieval:pop(1),
	    do_real_exit(Reason);

	%% The interpreted code terminated due to
	%% own exit(),bad match, etc. 
	{'EXIT',{confirmed,Reason}} ->
	    Msg_handler ! {sys,self(),{exit,Reason}},
	    dbg_iserver_api:set_state(idle),
	    tell_att_if_break(idle),
	    dbg_ieval:pop(1),
	    wait_int(Msg_handler);

	{'EXIT',Reason} ->
	    Msg_handler ! {sys,self(),{exit,Reason}},
	    dbg_iserver_api:set_state(idle),
	    tell_att_if_break(idle),
	    dbg_ieval:pop(1),
	    wait_int(Msg_handler);

	%% FIXME: WHy does this clause exist
	Throwed ->
	    dbg_iserver_api:set_state(idle),
	    tell_att_if_break(idle),
	    dbg_ieval:pop(1),
	    wait_int(Msg_handler)
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
	{ok,true} ->
	    all;
	{ok,Flag} ->
	    Flag;
	_ ->
	    false
    end.

wait_int(Msg_handler) ->
    receive
	%% Re-entry to the META process at top level
	{error_handler,Msg_handler,{eval,{M,F,As}}} ->
	    new_catch_lev(),
	    dbg_iserver_api:set_state(running),
	    tell_att_if_break(running),

	    %% Tell the attach window to update source code.
	    dbg_icmd:tell_attached({self(),re_entry, M, F}),
	    


	    case get(trace) of
		true ->
		    dbg_ieval:trace_out(
		      io_lib:format('++ (1) ~w:~w(~s)~n',
				    [M,F,dbg_ieval:format_args(As)]));
		_ ->
		    true
	    end,

	    %% "Same" as in int()
	    case catch dbg_ieval:eval_function(M,F,As,[],
					 M,false,1,extern,-1,extern) of
		{value,Val,Bs} ->
		    Msg_handler ! {sys,self(),{ready,Val}},
		    del_catch_lev(),
		    dbg_iserver_api:set_state(idle),
		    tell_att_if_break(idle),
		    wait_int(Msg_handler);
		{'EXIT',{Msg_handler,Reason}} ->
		    dbg_ieval:pop(1),
		    do_real_exit(Reason);
		{'EXIT',{confirmed,Reason}} ->
		    Msg_handler ! {sys,self(),{exit,Reason}},
		    del_catch_lev(),
		    dbg_iserver_api:set_state(idle),
		    tell_att_if_break(idle),
		    dbg_ieval:pop(1),
		    wait_int(Msg_handler);

		{'EXIT',Reason} ->
		    Msg_handler ! {sys,self(),{exit,Reason}},
		    del_catch_lev(),
		    dbg_iserver_api:set_state(idle),
		    tell_att_if_break(idle),
		    dbg_ieval:pop(1),
		    wait_int(Msg_handler);
		Throwed ->
		    del_catch_lev(),
		    dbg_iserver_api:set_state(idle),
		    tell_att_if_break(idle),
		    dbg_ieval:pop(1),
		    wait_int(Msg_handler)
	    end;
	%% Signal received from dying interpreted process
	%% (Due to exit in non-interpreted code)
	{'EXIT',Msg_handler,Reason} ->
	    do_real_exit(Reason);

	%% Catch signals from other linked processes.
	{'EXIT',Pid,Reason} ->
	    case dbg_icmd:attached_p(Pid) of
		true ->
		    dbg_icmd:detach(Pid),
		    wait_int(Msg_handler);
		_ ->
		    do_real_exit(Reason)
	    end;
	{sys,Msg_handler,{exited_nocatch,Reason}} ->
	    Msg_handler ! {sys,self(),{exit,Reason}},  % Yes, you shall die !!
	    wait_int(Msg_handler);
	{attach,Msg_handler,AttPid} ->
	    dbg_icmd:attach(AttPid),
	    wait_int(Msg_handler);
	{break_msg,Type,Data} ->
	    dbg_icmd:break_msg(Type,Data),
	    wait_int(Msg_handler);

	%% The code in Module is no longer interpreted. This
	%% process is 'idle', so we just remove it.
	{old_code, Module} ->
	    erase(cache),
	    erase([Module|db]),
	    wait_int(Msg_handler);



	%%FIXME: Handle new code
	%% The code in Module has be re-interpreted, we must handle this.
	{new_code, Module } ->				
	    %% Rensa cache
	    erase(cache), 

	    %% Ta bort modulreferenspekare (cahche:ad)
	    erase([Module|db]),

	    %% Make sure all {break_at...} messages also contain the
	    %% module reference.  In this (idle) state, confirm to
	    %% dbg_idb (in all other cases, reject).  But: Temporarily
	    %% send a message to the attached processes to tell them
	    %% to update their module reference.

	    wait_int(Msg_handler);


	%% Handle commands from attach process that are
	%% allowed in "idle" state.
	Cmd when tuple(Cmd), element(1,Cmd) == cmd ->
	    dbg_icmd:idle_wait(Cmd),
	    wait_int(Msg_handler)
    end.

do_real_exit(Reason) ->
    case get(error) of
	{Reason,Where,Bs,Stack} ->
	    %% Send exit-informatio to attech-process, and others.
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
    
create_catch_lev() ->
    put(catch_lev,[0]).

get_catch_lev() ->
    [Lev|_] = get(catch_lev),
    Lev.

new_catch_lev() ->
    put(catch_lev,[0|get(catch_lev)]).

del_catch_lev() ->
    [_|Lev] = get(catch_lev),
    put(catch_lev,Lev).

add_catch_lev() ->
    [Cur|Levs] = get(catch_lev),
    put(catch_lev,[Cur+1|Levs]).
	    
dec_catch_lev() ->
    [Cur|Levs] = get(catch_lev),
    put(catch_lev,[Cur-1|Levs]).

%% Look up the clauses for a fun.
%%
%% Returns: {Clauses,Mod,Name,Args} | not_interpreted | undef | badfun | badarity

lambda(Fun, As0) when function(Fun) ->
    {module,Mod} = erlang:fun_info(Fun, module),
    {index,I} = erlang:fun_info(Fun, index),
    {uniq,Uniq} = erlang:fun_info(Fun, uniq),
    Key = {'fun',Mod,I,Uniq},
    case fun_call_clauses(Mod, Key) of
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

fun_call_clauses(Mod, Key) ->
    case cached(Key) of
	false ->
	    case db_ref(Mod) of
		not_found ->
		    check_if_interpreted(Mod);
		Db ->
		    case dbg_idb:lookup(Db, Key) of
			{ok,Data} ->
			    cache(Key, Data),
			    Data;
			Error ->
			    case dbg_idb:lookup(Db, module) of
				{ok,_} -> undef;
				Error -> check_if_interpreted(Mod)
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
		not_found ->
		    check_if_interpreted(Mod);
		Db ->
		    case dbg_idb:lookup(Db, {Mod,Name,Arity,true}) of
			{ok,Data} ->
			    cache(Key, {true,Data}),
			    Data;
			Error ->
			    case dbg_idb:lookup(Db, module) of
				{ok,_} -> undef;
				Error -> check_if_interpreted(Mod)
			    end
		    end
	    end;
	{true,Cs} ->
	    Cs;
	{false,Cs} ->
	    undef
    end.

%% Sanity check: Should be called when a module was not found in our database.
%% Make sure that it is not interpreted.

check_if_interpreted(Mod) ->
    case code:interpreted(Mod) of
	true ->
	    error_msg("** ~w: module interpreted but not loaded **~n", [Mod]),
	    undef;
	false ->
	    not_interpreted
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
    case get(state) of
	break ->
	    dbg_icmd:tell_attached({self(),Info});
	_     ->
	    true
    end.
