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
-module(dbg_imeta).

%% External exports
-export([eval/3, exit_info/5]).

%% External exports used by dbg_ieval
-export([main_meta_loop/7]).

%-define(DBGDBG, 1).
-ifdef(DBGDBG).
-define(DBG(F,A), io:format("~p~p[~p]:" ++ F,[self(),?MODULE,?LINE] ++ A)).
-else.
-define(DBG(F,A), ok).
-endif.


%%====================================================================
%% External exports
%%====================================================================

%%--------------------------------------------------------------------
%% eval(Mod, Func, Args) -> Meta
%%   Mod = Func = atom()
%%   Args = [term()]
%%   Meta = pid()
%% Entry point from process being debugged.
%% Immediately returns the pid for the meta process.
%% The evaluated value will later be sent as a message to
%% process that called this function.
%%--------------------------------------------------------------------
eval(Mod, Func, Args) ->
    Debugged = self(),
    Int = dbg_iserver:find(),
    ?DBG("Eval ~p:~p~n", [Mod, Func]),
    case dbg_iserver:call(Int, {get_meta, Debugged}) of
	{ok, Meta} ->
	    Meta ! {error_handler,Debugged,{eval,{Mod,Func,Args}}},
	    Meta;
	{error, not_interpreted} ->
	    spawn_link(fun() -> int(Int, Debugged, Mod, Func, Args) end)
    end.

%%--------------------------------------------------------------------
%% exit_info(Int, AttPid, OrigPid, Reason, Info)
%%  Int = AttPid = OrigPid = pid()
%%  Reason = term()
%%  Info = {{Mod,Line}, Bs, Stack} | {}
%% Meta process started when attaching to a terminated process.
%%--------------------------------------------------------------------
exit_info(Int, AttPid, OrigPid, Reason, Info) ->
    put(int, Int),
    put(attached, [AttPid]),
    put(breakpoints, dbg_iserver:call(Int, all_breaks)),
    put(self, OrigPid),
    
    case Info of
	{{Mod,Line},Bs,Stack} ->
	    S = binary_to_term(Stack),
	    put(stack, S),
	    Sp = sp(S),
	    dbg_icmd:tell_attached({exit_at, {Mod, Line}, Reason, Sp+1}),
	    exit_loop(OrigPid, Reason, Mod, Line, Bs);
	_ ->
	    put(stack, []),
	    dbg_icmd:tell_attached({exit_at, null, Reason, 1}),
	    exit_loop(OrigPid, Reason, null, null, [])
    end.


%%====================================================================
%% Internal functions
%%====================================================================

%% Entry point for first-time initialization of meta process
int(Int, Debugged, M, F, As) ->
    process_flag(trap_exit, true),

    %% Inform dbg_iserver, get the initial status in return
    Status = dbg_iserver:call(Int,
			      {new_process, Debugged, self(), {M, F, As}}),

    %% Initiate process dictionary
    put(int, Int),
    put(attached, []),
    put(breakpoints, dbg_iserver:call(Int, all_breaks)),
    put(cache, []),
    put(catch_lev, []),
    put(error, none),
    put(next_break, Status),
    put(self, Debugged),
    put(stack, []),
    put(stack_trace, dbg_iserver:call(Int, get_stack_trace)),
    put(trace, false),
    put(user_eval, []),
    
    eval_mfa(Debugged, M, F, As, 1),

    dbg_iserver:cast(Int, {set_status, self(), idle, {}}),
    dbg_icmd:tell_attached_if_break(idle),

    main_meta_loop(Debugged, [], 1, false, extern, -1, extern).

eval_mfa(Debugged, M, F, As, Le) ->
    dbg_ieval:trace(call, {Le,none,M,F,As}),
    ?DBG("eval_mfa[~p] ~p:~p ~n", [Le, M, F]),
    dbg_ieval:init_catch_lev(),
    Res = dbg_ieval:eval_function(M, F, As, Le),
    dbg_ieval:exit_catch_lev(),
    ?DBG("eval_res[~p] ~p:~p => ~p~n", [Le,M,F,Res]),
    case Res of
	{value, Val, _Bs} ->
	    Debugged ! {sys, self(), {ready, Val}};

	{'EXIT', {Debugged, Reason}} ->
	    dbg_ieval:pop(Le),
	    if
		Le>1 -> exit({Debugged, Reason});
		true -> do_real_exit(Reason)
	    end;

	{'EXIT', {confirmed, Reason}} ->
	    Debugged ! {sys, self(), {exit, Reason}},
	    dbg_ieval:pop(Le);

	{'EXIT', {int, Reason}} -> % Interpreter has terminated
	    exit(Reason);

	{'EXIT', Reason} ->
	    Debugged ! {sys, self(), {exit, Reason}},
	    if   %% qqqq
		Le > 1 ->
		    receive
			{sys, Debugged, {exited_nocatch, Reason}} ->
			    Debugged ! {sys, self(), {exit, Reason}}
		    end;
		true -> ignore
	    end,	    
	    dbg_ieval:pop(Le);
	_Value ->  %% Thrown value
%	    Debugged ! {sys, self(), {throw, Value}}, % qqqq
	    dbg_ieval:pop(Le)
    end.

%%--Loops-------------------------------------------------------------

%% main_meta_loop(Debugged, Bs, Le, Lc, Cm, Line, F) -> {value, Value, Bs}
%%   Debugged = pid()      Debugged process
%%   Bs = [{Var,Val}]      Bindings
%%   Le = integer()        Level
%%   Lc = false            ?
%%   Cm = extern | atom()  Current module
%%   Line = integer() -1   Line number
%%   F = extern            ?
%%   Value = term()
%% Main receive loop for the meta process.
main_meta_loop(Debugged, Bs, Le, Lc, Cm, Line, F) when integer(Le) ->
    ?DBG("Loop [~p] ~n", [Le]),
    receive	
	%% The following messages can only be received when Meta is
	%% waiting for Debugged to evaluate non-interpreted code
	%% or a BIF. Le>1.
	{sys, Debugged, {apply_result, Val}} when Lc==false ->
	    ?DBG("Loop [~p] apply_result~n", [Le]),
	    dbg_ieval:trace(return, {Le,Val,Lc}),
	    {value, Val, Bs};
	{sys, Debugged, {apply_result, Val}} ->
	    ?DBG("Loop [~p] apply_result~n", [Le]),
	    {value, Val, Bs};
	{sys, Debugged, {eval_result, Val, Bs2}} ->
	    ?DBG("Loop [~p] eval_result~n", [Le]),
	    {value, Val, Bs2};
	{sys, Debugged, {thrown, Value}} ->
	    ?DBG("Loop [~p] thrown~n", [Le]),
	    throw(Value);
	{sys, Debugged, {thrown_nocatch, Value}} ->
	    ?DBG("Loop [~p] thrown_nocatch~n", [Le]),
	    throw(Value, Cm, Line, Bs);

	%% The following messages can be received any time.
	%% If Le==1, Meta is at the top level.
	%% If Le>1, Meta has been entered more than once from
	%% the error_handler module.
	{sys, Debugged, {exited_nocatch,Reason}} when Le==1->
	    ?DBG("Loop [~p] exited_nocatch~n", [Le]),
	    Debugged ! {sys, self(), {exit, Reason}},
	    main_meta_loop(Debugged, Bs, Le, Lc, Cm, Line, F);
	{sys, Debugged, {exited_nocatch, Reason}} ->
	    %% dbg_ieval:put_error(Reason, Cm, Line, Bs),
	    %% exit({confirmed, Reason});
	    ?DBG("Loop [~p] exited_nocatch~n", [Le]),
	    dbg_ieval:exit({confirmed, Reason}, Cm, Line, Bs);

	%% Re-entry to Meta at top level.
	{error_handler, Debugged, {eval, {Mod,Func,Args}}} when Le==1 ->
	    ?DBG("Loop [~p] reentry ~p~n", [Le,{Mod,Func}]),
	    dbg_iserver:cast(get(int), {set_status, self(), running, {}}),
	    dbg_icmd:tell_attached_if_break(running),
	    %% Tell attached process(es) to update source code.
	    dbg_icmd:tell_attached({re_entry, Mod, Func}),
	    eval_mfa(Debugged, Mod, Func, Args, 1),
	    dbg_iserver:cast(get(int), {set_status, self(), idle, {}}),
	    dbg_icmd:tell_attached_if_break(idle),
	    main_meta_loop(Debugged, Bs, Le, Lc, Cm, Line, F);
	{error_handler, Debugged, {eval, {Mod,Func,Args}}} ->
	    ?DBG("Loop [~p] reentry ~p~n", [Le, {Mod,Func}]),
	    eval_mfa(Debugged, Mod, Func, Args, Le),
	    main_meta_loop(Debugged, Bs, Le, Lc, Cm, Line, F);

	%% Signal received from dying interpreted process
	%% (due to exit in non-interpreted code).
	{'EXIT', Debugged, Reason} when Le==1 ->
	    ?DBG("Loop [~p] EXIT~n", [Le]),
	    do_real_exit(Reason);
	{'EXIT', Debugged, Reason} ->
	    ?DBG("Loop [~p] EXIT~n", [Le]),
	    dbg_ieval:exit(Debugged, Reason, Cm, Line, Bs);

	%% Interpreter has terminated.
	%% XXX Can we be sure that the interpreter has terminated?
	%% It could be another process.
	{'EXIT',_Pid,Reason} ->
	    ?DBG("Loop [~p] EXIT~n", [Le]),
	    exit(Reason);

	Msg ->
	    ?DBG("Loop [~p] Msg ~p~n", [Le, Msg]),
	    dbg_icmd:handle_msg(Msg, {main, Bs, Le, Cm, Line}),
	    main_meta_loop(Debugged, Bs, Le, Lc, Cm, Line, F)
    end.

exit_loop(OrigPid, Reason, Mod, Line, Bs) ->
    receive
	Msg ->
	    dbg_icmd:handle_msg(Msg, {exit, Bs, null, Mod, Line}),
	    exit_loop(OrigPid, Reason, Mod, Line, Bs)
    end.
    
%%--------------------------------------------------------------------

do_real_exit(Reason) ->
    case get(error) of
	{Reason, Where, Bs, Stack} ->
	    exit({self(), Reason, Where, Bs, Stack});
	_ ->
	    exit({self(), Reason})
    end.

sp([]) -> 0;
sp(S) -> element(1, hd(S)).

throw(Value, Cm, Line, Bs) ->
    dbg_ieval:do_put_error(nocatch, Cm, Line, Bs),
    throw(Value).
