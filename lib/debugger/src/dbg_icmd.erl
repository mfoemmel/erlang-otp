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
-module(dbg_icmd).

%% Internal command receiver/handler
-export([cmd/5]).

%% User control of process execution and settings
-export([step/1, next/1, continue/1, finish/1, skip/1, timeout/1, stop/2]).
-export([eval/2]).
-export([set/3, get/3]).
-export([handle_msg/2]).

%% Library functions for attached process handling
-export([tell_attached/1, tell_attached/2, tell_attached_if_break/1]).

%% get_binding/2
-export([get_binding/2]).

%%====================================================================
%% Internal command receiver/handler
%%====================================================================

%%--------------------------------------------------------------------
%% cmd(Expr, Bs, Cm, Le, F) -> {skip, Bs} | Bs
%% This function is called from dbg_ieval before evaluating any expression
%% to give the user the chance to inspect variables etc.
%% get(next_break) => init_break | break | running | Next == CallLevel
%%                  | Finish == {Next,Function}
%% specifies if the process should break.
%%--------------------------------------------------------------------
cmd(Expr, Bs, Cm, Le, F) ->
    cmd(Expr, Bs, Cm, Le, F, get(next_break)).

cmd(_Expr, Bs, {none}, _Le, _F, _NextBreak) ->
    Bs;

cmd(Expr, Bs, Cm, Le, F, init_break) ->
    put(next_break, break),
    break(Expr, Bs, Cm, Le, F);
cmd(Expr, Bs, Cm, Le, F, break) ->
    break(Expr, Bs, Cm, Le, F);
cmd(Expr, Bs, Cm, Le, F, running) ->
    LineNo = element(2, Expr),
    case break_p(Cm, LineNo, Le, Bs) of
	true ->
	    put(next_break, break),
	    break(Expr, Bs, Cm, Le, F);
	false ->
	    handle_cmd(Bs, Cm, Le, running, LineNo, F)
    end;

cmd(Expr, Bs, Cm, Le, F, Next) when integer(Next), Next<Le ->
    LineNo = element(2, Expr),
    handle_cmd(Bs, Cm, Le, Next, LineNo, F);
cmd(Expr, Bs, Cm, Le, F, Next) when integer(Next), Next>=Le ->
    put(next_break, break),
    break(Expr, Bs, Cm, Le, F);

cmd(Expr, Bs, Cm, Le, F, {Next,FP}) when integer(Next), Next<Le ->
    LineNo = element(2, Expr),
    handle_cmd(Bs, Cm, Le, {Next,FP}, LineNo, F);
cmd(Expr, Bs, Cm, Le, F, {Next,F}) when integer(Next), Next>=Le ->
    LineNo = element(2, Expr),
    handle_cmd(Bs, Cm, Le, {Next,F}, LineNo, F);
cmd(Expr, Bs, Cm, Le, F, {Next,_}) when integer(Next), Next>=Le ->
    LineNo = element(2, Expr),
    put(next_break, break),
    break(Expr, Bs, Cm, Le, F).

break(Expr, Bs, Cm, Le, F) ->
    LineNo = element(2, Expr),
    tell_attached({break_at, Cm, LineNo, Le}),
    dbg_iserver:cast(get(int), {set_status, self(), break, {Cm, LineNo}}),
    handle_cmd(Bs, Cm, Le, break, LineNo, F).

%% handle_cmd/6 - Loops for a while  but finally returns a list of bindings.
handle_cmd(Bs, Cm, Le, break, LineNo, F) ->
    receive
	{user, {cmd, Cmd}} ->
	    dbg_iserver:cast(get(int), {set_status, self(), running, {}}),
	    tell_attached(running),
	    case Cmd of
		step -> Bs;
		next -> put(next_break, Le), Bs;
		continue -> put(next_break, running), Bs;
		finish -> put(next_break, {Le,F}), Bs;
		skip -> {skip, Bs}
	    end;
	{user, {eval, Cmd}} ->
	    Bs1 = eval_nonrestricted(Cmd, Bs, Cm, Le, LineNo, F),
	    handle_cmd(Bs1, Cm, Le, break, LineNo, F);

	{'EXIT', Pid, Reason} ->
	    case get(self) of
		Pid ->   % Debugged process has terminated
		    dbg_ieval:exit(Pid,Reason,Cm,LineNo,Bs);
		_ ->     % Interpreter has terminated
		    exit(Reason)
	    end;
	
	Msg ->
	    handle_msg(Msg, {break, Bs, Le, Cm, LineNo}),
	    handle_cmd(Bs, Cm, Le, break, LineNo, F)
    end;
handle_cmd(Bs, Cm, Le, State, LineNo, F) -> % State=running|Next|{Next,Fp}
    receive
	{'EXIT', Pid, Reason} ->
	    case get(self) of
		Pid ->   % Debugged process has terminated
		    dbg_ieval:exit(Pid,Reason,Cm,LineNo,Bs);
		_ ->     % Interpreter has terminated
		    exit(Reason)
	    end;
	
	Msg ->
	    handle_msg(Msg, {State, Bs, Le, Cm, LineNo}),
	    handle_cmd(Bs, Cm, Le, break, LineNo, F)

	after 0 -> Bs
	end.

break_p(Mod, Line, Le, Bs) ->
    case lists:keysearch({Mod, Line}, 1, get(breakpoints)) of
	{value, {_Point, [active, Action, _, Cond]}} ->
	    case get(user_eval) of
		[{Line, Le}|_] -> false;
		_ ->
		    Bool = case Cond of
			       null -> true;
			       {CM, CN} -> catch apply(CM, CN, [Bs])
			   end,
		    if
			Bool==true ->
			    case Action of
				enable -> ignore;
				disable ->
				    dbg_iserver:cast(get(int),
						     {break_option,
						      {Mod, Line},
						      status,
						      inactive});
				delete ->
				    dbg_iserver:cast(get(int),
						     {delete_break,
						      {Mod, Line}})
			    end;
			true -> ignore
		    end,
		    Bool
	    end;
	_Other -> % {value, {_Point, [inactive|_]}} | false
	    false
    end.


%%====================================================================
%% User control of process execution and settings
%%====================================================================

step(Meta) ->     Meta ! {user, {cmd, step}}.
next(Meta) ->     Meta ! {user, {cmd, next}}.
continue(Meta) -> Meta ! {user, {cmd, continue}}.
finish(Meta) ->   Meta ! {user, {cmd, finish}}.
skip(Meta) ->     Meta ! {user, {cmd, skip}}.

timeout(Meta) -> Meta ! timeout.
stop(Meta, AttPid) -> Meta ! {int, {attached, AttPid}}.

eval(Meta, {Mod, Cmd}) ->
    eval(Meta, {Mod, Cmd, nostack});
eval(Meta, {Mod, Cmd, SP}) ->
    Cmd2 = case lists:reverse(Cmd) of    % Commands must end with ".\n"
	       [10,$.|_] -> Cmd;
	       [10|T] -> lists:reverse([10,$.|T]);
	       [$.|T] -> lists:reverse([10,$.|T]);
	       T -> lists:reverse([10,$.|T])
	   end,
    Meta ! {user, {eval, {self(), Mod, Cmd2, SP}}}.

%% Tag           Args
%% ---           ----
%% trace         Bool
%% stack_trace   Flag
set(Meta, Tag, Args) ->
    Meta ! {user, {set, Tag, Args}}.

%% Tag           Args
%% ---           ----
%% bindings      SP
%% stack_frame   {Dir, SP}
%% messages      null
%% backtrace     N
get(Meta, Tag, Args) ->
    Meta ! {user, {get, Tag, self(), Args}},
    receive
	{Meta, Tag, Reply} -> Reply
    end.

handle_msg({int, Msg}, Info) ->
    handle_int_msg(Msg, Info);
handle_msg({user, Msg}, Info) ->
    handle_user_msg(Msg, Info);
handle_msg(Msg, Info) ->
    io:format("***WARNING*** Unexp msg ~p, info ~p~n", [Msg, Info]).

handle_int_msg({attached, AttPid}, {Status, Bs, Le, Cm, Line}) ->
    if
	Status==main, Le==1 ->
	    attach(AttPid);
	Status==running;
	integer(Status);
	tuple(Status) ->
	    attach(AttPid, Cm, Line);
	true ->
	    attach(AttPid, Cm, Line),
	    Msg = case Status of
		      main -> {func_at, Cm, Line, Le};
		      break -> {break_at, Cm, Line, Le};
		      wait -> {wait_at, Cm, Line, Le};
		      wait_after -> {wait_after_at, Cm, Line, Le}
		  end,
	    tell_attached(AttPid, Msg)
    end;
handle_int_msg({detached, AttPid}, _Status) ->
    detach(AttPid);
handle_int_msg({old_code, Mod}, {Status, Bs, Le, Cm, Line}) ->
    if
	Status==main, Le==1 ->
	    erase([Mod|db]),
	    put(cache, []);
	true ->
	    case dbg_ieval:in_use_p(Mod, Cm) of
		true ->
		    %% A call to Mod is on the stack (or might be),
		    %% so we must terminate.
		    exit(get(self), kill),
		    dbg_ieval:exit(old_code, Cm, Line, Bs);
		false ->
		    erase([Mod|db]),
		    put(cache, [])
	    end
    end;
handle_int_msg({new_break, Break}, Info) ->
    put(breakpoints, [Break | get(breakpoints)]);
handle_int_msg({delete_break, Point}, Info) ->
    put(breakpoints, lists:keydelete(Point, 1, get(breakpoints)));
handle_int_msg({break_options, Break}, Info) ->
    {Point, _Options} = Break,
    put(breakpoints, lists:keyreplace(Point, 1, get(breakpoints), Break));
handle_int_msg(no_break, Info) ->
    put(breakpoints, []);
handle_int_msg({no_break, Mod}, Info) ->
    put(breakpoints, lists:filter(fun({{M,_L},_Os}) ->
					  if
					      M==Mod -> false;
					      true -> true
					  end
				  end,
				  get(breakpoints)));

handle_int_msg(stop, {exit, Bs, Le, Cm, Line}) ->
    exit(normal).

handle_user_msg({eval, Cmd}, {wait, Bs, Le, Cm, Line}) ->
    eval_restricted(Cmd, Bs);
handle_user_msg({eval, Cmd}, {wait_after, Bs, Le, Cm, Line}) ->
    eval_restricted(Cmd, Bs);

handle_user_msg({set, trace, Bool}, Info) ->
    set_trace(Bool);
handle_user_msg({set, stack_trace, Flag}, Info) ->
    set_stack_trace(Flag);

handle_user_msg({get, bindings, From, SP}, {Status, Bs, Le, Cm, Line}) ->
    reply(From, bindings, bindings(Bs, SP));
handle_user_msg({get, stack_frame, From, {Dir, SP}}, Info) ->
    reply(From, stack_frame, stack_frame(Dir, SP));
handle_user_msg({get, messages, From, _}, Info) ->
    reply(From, messages, messages());
handle_user_msg({get, backtrace, From, N}, Info) ->
    reply(From, backtrace, backtrace(N)).

reply(From, Tag, Reply) ->
    From ! {self(), Tag, Reply}.

set_trace(Bool) ->
    put(trace, Bool),
    tell_attached({trace, Bool}).

set_stack_trace(true) ->
    set_stack_trace(all);
set_stack_trace(Flag) ->    
    if
	Flag==false -> erase(stack);
	true ->
	    case get(stack) of
		undefined -> put(stack, []);
		Stack -> ignore
	    end
    end,
    put(stack_trace, Flag),
    tell_attached({stack_trace, Flag}).

bindings(Bs, nostack) ->
    Bs;
bindings(Bs, SP) ->
    bindings(Bs, SP, get(stack)).

bindings(Bs, SP, Stack) when SP>element(1, hd(Stack)) ->
    Bs;
bindings(_Bs, SP, Stack) ->
    binding(SP, Stack).

binding(SP, [{SP,{_,_,_,Bs}}|_]) ->
    Bs;
binding(SP, [{Le,_}|_]) when SP>Le ->
    [];
binding(SP, [_|Stack]) ->
    binding(SP, Stack);
binding(_SP, []) ->
    [].

stack_frame(up, SP) ->
    stack_frame_up(SP-1, get(stack));
stack_frame(down, SP) ->
    stack_frame_down(SP+1, get(stack), bottom).

stack_frame_up(SP, []) ->
    top;
stack_frame_up(SP, [{Level, Frame} | Stack]) when SP<Level ->
    stack_frame_up(SP, Stack);
stack_frame_up(SP, [{Level, {Mod, _, Line, _}} | _Stack]) ->
    {Level, Mod, Line}.

stack_frame_down(SP, [{Level, Frame} | Stack], _Above) when SP<Level ->
    stack_frame_down(SP, Stack, {Level,Frame});
stack_frame_down(SP, [{SP, {Mod, _, Line, _}} | _Stack], _Above) ->
    {SP, Mod, Line};
stack_frame_down(SP, _Stack, bottom) ->
    bottom;
stack_frame_down(SP, _Stack, {Level, {Mod, _, Line, _}}) ->
    {Level, Mod, Line}.

messages() ->
    {messages, Msgs} = erlang:process_info(get(self), messages),
    Msgs.

backtrace(all) ->
    get(stack);
backtrace(N) ->
    lists:sublist(get(stack), N).


%%====================================================================
%% Evaluating expressions withing process context
%%====================================================================

eval_restricted({From, Mod, Cmd, SP}, Bs) ->
    case catch parse_cmd(Cmd, 1) of
	{'EXIT', _Reason} ->
	    From ! {self(), {eval_rsp, 'Parse error'}};
	[{var,_,Var}] ->
	    Bs2 = bindings(Bs, SP),
	    Res = case get_binding(Var, Bs2) of
		      {value, Value} -> Value;
		      unbound -> unbound
		  end,
	    From ! {self(), {eval_rsp, Res}};
	_Forms ->
	    From ! {self(), {eval_rsp,'Only possible to inspect variables'}}
    end.

eval_nonrestricted({From, Mod, Cmd, SP}, Bs, Cm, Le, LineNo, F) when SP<Le->
    %% Evaluate in stack.
    eval_restricted({From, Mod, Cmd, SP}, Bs),
    Bs;
eval_nonrestricted({From, Mod, Cmd, SP}, Bs, Cm, Le, LineNo, F) ->
    case catch parse_cmd(Cmd, LineNo) of
	{'EXIT', _Reason} ->
	    From ! {self(), {eval_rsp, 'Parse error'}},
	    Bs;
	Forms ->
	    mark_running(LineNo, Le),
	    {Res, Bs2} =
		lists:foldl(fun(Expr, {_Res, Bs0}) ->
				    eval_nonrestricted(Expr, Bs0, Cm, Le, F)
			    end,
			    {null, Bs},
			    Forms),
	    mark_break(Cm, LineNo, Le),
	    From ! {self(), {eval_rsp, Res}},
	    Bs2
    end.

eval_nonrestricted({match,_,{var,_,Var},Expr}, Bs, Cm, Le, F) ->
    {value, Res, Bs2} = dbg_ieval:eval_expr(Expr, Bs, Cm, false, Le, F),
    Bs3 = case lists:keysearch(Var, 1, Bs) of
	      {value, {Var, _Value}} ->
		  lists:keyreplace(Var, 1, Bs2, {Var, Res});
	      false -> [{Var, Res} | Bs2]
	  end,
    {Res, Bs3};
eval_nonrestricted({var,_,Var}, Bs, Cm, Le, F) ->
    Res = case lists:keysearch(Var, 1, Bs) of
	      {value, {Var, Value}} -> Value;
	      false -> unbound
	  end,
    {Res, Bs};
eval_nonrestricted(Expr, Bs, Cm, Le, F) ->
    {value, Res, Bs2} = dbg_ieval:eval_expr(Expr, Bs, Cm, false, Le, F),
    {Res, Bs2}.

mark_running(LineNo, Le) ->
    put(next_break, running),
    put(user_eval, [{LineNo, Le} | get(user_eval)]),
    dbg_iserver:cast(get(int), {set_status, self(), running, {}}),
    tell_attached(running).

mark_break(Cm, LineNo, Le) ->
    put(next_break, break),
    put(user_eval, tl(get(user_eval))),
    tell_attached({break_at, Cm, LineNo, Le}),
    dbg_iserver:cast(get(int), {set_status, self(), break, {Cm, LineNo}}).

parse_cmd(Cmd, LineNo) ->
    {ok, Tokens, Pos} = erl_scan:string(Cmd, LineNo),
    {ok, Forms} = erl_parse:parse_exprs(Tokens),
    Forms.


%%====================================================================
%% Library functions for attached process handling
%%====================================================================

attach(AttPid) ->     % Idle waiting for new call
    attach(AttPid, null, null).
attach(AttPid, Mod, Line) ->
    put(next_break, break),
    case lists:member(AttPid, get(attached)) of
	true -> ignore;
	false -> put(attached, [AttPid | get(attached)])
    end,
    tell_attached(AttPid, {attached, Mod, Line, get(trace)}).

detach(AttPid) ->
    Attached = lists:delete(AttPid, get(attached)),
    put(attached, Attached),
    if
	Attached==[] -> set_trace(false);
	true -> ignore
    end.

tell_attached(Msg) ->
    lists:foreach(fun(AttPid) -> tell_attached(AttPid, Msg) end,
		  get(attached)).

tell_attached(AttPid, Msg) ->
    AttPid ! {self(), Msg}.

tell_attached_if_break(Msg) ->
    case get(next_break) of
	break -> tell_attached(Msg);
	_ -> ignore
    end.


%%====================================================================
%% get_binding/2
%%====================================================================

get_binding(Var, Bs) ->
    case lists:keysearch(Var, 1, Bs) of
	{value, {Var, Value}} -> {value, Value};
	false -> unbound
    end.
