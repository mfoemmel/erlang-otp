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

-module(dbg_icmd).

-export([break/2,delete_break/2,no_break/0,no_break/1,
	 break_in/3,break_in/4,del_break_in/3,disable_break/2,
	 enable_break/2,test_at_break/3,action_at_break/3]).

-export([continue/1,next/1,finish/1,step/1,skip/1,inspect/2,inspect/3,
	 inspect_def/3,messages/1,timeout/1,command/4,command/5,
	 up_stack/2,down_stack/2,backtrace/1,backtrace/2]).

-export([trace/1,trace_pid/2,stack_trace/1,stack_trace_pid/2]).

-export([get_binding/2]).

%%%------------------------------------------------------
%%% Internal exports.
%%%------------------------------------------------------

-export([cmd/5,attach/1,attach/3,attached_p/1,detach/1,
	 init_breaks/0,break_msg/2,cmd_rec/3,idle_wait/1,
	 wait_cmd/2,exit_cmd/2,new_break1/2,delete_break1/1,
	 no_break2/1,update_break/3]).

-export([tell_attached/1]).

%%%------------------------------------------------------
%%% User interface for commands towards interpreted
%%% processes.
%%%------------------------------------------------------

break(Mod, Line) ->
    new_break({Mod,Line}).

break(Mod, Line, Cond) ->  %% Not exported ...
    new_break({Mod,Line},Cond).

delete_break(Mod, Line) ->
    delete_break({Mod,Line}).

no_break() ->
    no_break1({}).

no_break(Mod) ->
    no_break1({Mod}).

break_in(Mod,Fnk,Arity) ->
    break_in1(Mod,Fnk,Arity).

break_in(Mod,Fnk,Arity,Cond) ->
    break_in1(Mod,Fnk,Arity,Cond).

del_break_in(Mod,Fnk,Arity) ->
    del_break_in1(Mod,Fnk,Arity).

disable_break(Mod,Line) ->
    disable_break1(Mod,Line).
    
enable_break(Mod,Line) ->
    enable_break1(Mod,Line).

test_at_break(Mod,Line,Fnk) ->
    test_at_break1(Mod,Line,Fnk).

action_at_break(Mod,Line,Action) ->
    action_at_break1(Mod,Line,Action).

continue(Meta) ->
    Meta ! {cmd, continue, {}}.

next(Meta) ->
    Meta ! {cmd, next, {}}.

finish(Meta) ->
    Meta ! {cmd, finish, {}}.

step(Meta) ->
    Meta ! {cmd, step, {}}.

skip(Meta) ->
    Meta ! {cmd, skip, {}}.

trace(Trace) ->
    trace_1(Trace).

trace_pid(Meta, on) ->
    Meta ! {cmd, trace, true};
trace_pid(Meta, off) ->
    Meta ! {cmd, trace, false}.

stack_trace(Flag) -> %% Flag == all (true), no_tail or false
    stack_trace_1(Flag).

stack_trace_pid(Meta, Flag) ->  %% Flag == all (true), no_tail or false
    Meta ! {cmd, stack_trace, Flag}.

inspect(Meta, Var) ->
    inspect(Meta,Var,nostack).

inspect(Meta, Var, SP) ->
    Meta ! {cmd, inspect, {self(),Var,SP}},
    receive
	{Meta, inspect_res, Result} ->
	    Result
    end.

inspect_def(Meta, Def, Mod) ->
    Meta ! {cmd, inspect_def, {self(),Def,Mod}},
    receive
	{Meta, inspect_def_res, Result} ->
	    Result
    end.

messages(Meta) ->
    Meta ! {cmd, messages, {self()}},
    receive
	{Meta, messages, Result} ->
	    Result
    end.

timeout(Meta) ->
    Meta ! {cmd, timeout, {}}.

up_stack(Meta,SP0) ->
    Meta ! {cmd, stack_frame, {self(),up,SP0}},
    receive
	{Meta, stack_frame, {SP,Mod,Line}} ->
	    {ok,{SP,Mod,Line}};
	{Meta, stack_frame, What} ->
	    {error,What}
    end.

down_stack(Meta,SP0) ->
    Meta ! {cmd, stack_frame, {self(),down,SP0}},
    receive
	{Meta, stack_frame, {SP,Mod,Line}} ->
	    {ok,{SP,Mod,Line}};
	{Meta, stack_frame, What} ->
	    {error,What}
    end.

%%-- Get the program stack for the Meta process.
%%-- Return a list of entrys (except the bindings).
%%--   [{CallLevel,Mod,Function,LineNo},...]
%%--    where Function == {Fnk,Arity} or extern.

backtrace(Meta) -> backtrace(Meta,all).
backtrace(Meta,N) ->
    Meta ! {cmd, backtrace, {self(),N}},
    receive
	{Meta, backtrace, BT} ->
	    {ok, BT}
    end.


command(Meta,Mod,Cmd,No) ->
    command(Meta,Mod,Cmd,No,nostack).

command(Meta,Mod,Cmd0,No,SP) ->
    Cmd = add_dot(lists:reverse(Cmd0)),
    Meta ! {cmd, command, {self(),Mod,Cmd,No,SP}},
    wait_for_response.    %% Response is the msg {Meta,command_resp,No,Result}

add_dot([10,$.|Rest]) -> lists:reverse([10,$.|Rest]);
add_dot([10|Rest])    -> lists:reverse([10,$.|Rest]);
add_dot([$.|Rest])    -> lists:reverse([10,$.|Rest]);
add_dot(Rest)         -> lists:reverse([10,$.|Rest]).

get_binding(Var, Bs) ->
    case lists:keysearch(Var, 1, Bs) of
	{value,{Var,Value}} ->
	    {value, Value};
	_ ->
	    unbound
    end.


%%%------------------------------------------------------
%%% Prepared to handle break points per process.
%%%------------------------------------------------------

%%break(Meta, Mod, Line) ->
%%    Meta ! {cmd, break, {Mod, Line}}.
%%
%%delete_break(Meta, Mod, Line) ->
%%    Meta ! {cmd, delete_break, {Mod, Line}}.
%%
%%no_break(Meta, Mod) ->
%%    Meta ! {cmd, no_break, {Mod}}.

%%% cmd/5 - Evaluate, then return a list of bindings.
%%%
%%%-- Internal command receiver/handler
%%%-- State = break, running, Next where Next == CallLevel
%%%-- or Finish where Finish == {Next,Function}
%%%-- there we shall break.
%%%------------------------------------------------------

cmd(Expr, Bs, Cm, Le, F) ->
    cmd(Expr, Bs, Cm, Le, F, get(state)).

cmd(_, Bs, {none}, _, _, _) ->
    Bs;
cmd(Expr, Bs, Cm, Le, F, running) ->
    LineNo = element(2, Expr),
    case break_p(Cm, LineNo, Le, Bs) of
	true ->
	    put(state, break),
	    cmd(Expr, Bs, Cm, Le, F, break);
	false ->
	    handle_cmd(Bs, Cm, Le, running, LineNo, F)
    end;
cmd(Expr, Bs, Cm, Le, F, Next) when integer(Next), Next < Le ->
    LineNo = element(2, Expr),
    handle_cmd(Bs, Cm, Le, Next, LineNo, F);
cmd(Expr, Bs, Cm, Le, F, Next) when integer(Next), Next >= Le ->
    LineNo = element(2, Expr),
    put(state, break),
    cmd(Expr, Bs, Cm, Le, F, break);
cmd(Expr, Bs, Cm, Le, F, {Next,FP}) when integer(Next), Next < Le ->
    LineNo = element(2, Expr),
    handle_cmd(Bs, Cm, Le, {Next,FP}, LineNo, F);
cmd(Expr, Bs, Cm, Le, F, {Next,F}) when integer(Next), Next >= Le ->
    LineNo = element(2, Expr),
    handle_cmd(Bs, Cm, Le, {Next,F}, LineNo, F);
cmd(Expr, Bs, Cm, Le, F, {Next,_}) when integer(Next), Next >= Le ->
    LineNo = element(2, Expr),
    put(state, break),
    cmd(Expr, Bs, Cm, Le, F, break);

cmd(Expr, Bs, Cm, Le, F, break) ->
    LineNo = element(2, Expr),
    AttP = case get(attached) of
	       [] -> 
		   true;
	       Attached ->
		   tell_attached(Attached,{self(),break_at,Cm,LineNo,Le}),
		   false
	   end,
    dbg_iserver_api:break_at(Cm, LineNo, AttP),
    handle_cmd(Bs, Cm, Le, break, LineNo, F);
cmd(Expr, Bs, Cm, Le, F, init_break) ->  %% Initial attached
    LineNo = element(2, Expr),
    tell_attached({self(),break_at,Cm,LineNo,Le}),
    dbg_iserver_api:break_at(Cm, LineNo, false),
    put(state,break),
    handle_cmd(Bs, Cm, Le, break, LineNo, F).

%% handle_cmd/6 - Loops for a while - but finally returns a list of bindings.
%% 

handle_cmd(Bs, Cm, Le, break, LineNo, F) ->
    receive
	{attach,Msg_handler,AttPid} ->
	    attach(AttPid,Cm,LineNo),
	    AttPid ! {self(),break_at,Cm,LineNo,Le},
	    handle_cmd(Bs, Cm, Le, break, LineNo, F);
        {cmd, continue, _} ->
	    dbg_iserver_api:set_state(running),
	    tell_attached({self(),running}),
	    put(state, running),
	    Bs;
	{cmd, next, _} ->
	    put(state, Le),    % Set the next level to break at!
	    dbg_iserver_api:set_state(running),
	    tell_attached({self(),running}),
	    Bs;
	{cmd, finish, _} ->
	    put(state, {Le,F}),  % End up execution in this function !
	    dbg_iserver_api:set_state(running),
	    tell_attached({self(),running}),
	    Bs;
	{cmd, break, {Mod, Line}} ->
	    new_break({Mod,Line}),
	    handle_cmd(Bs, Cm, Le, break, LineNo, F);
	{cmd, delete_break, {Mod, Line}} ->
	    delete_break({Mod, Line}),
	    handle_cmd(Bs, Cm, Le, break, LineNo, F);
	{cmd, no_break, Mod} ->
	    no_break1(Mod),
	    handle_cmd(Bs, Cm, Le, break, LineNo, F);
	{cmd, step, _} ->
	    dbg_iserver_api:set_state(running),
	    tell_attached({self(),running}),
	    Bs;
	{cmd, skip, _} ->   % Do not execute expression :-)
	    dbg_iserver_api:set_state(running),
	    tell_attached({self(),running}),
	    {skip,Bs};
	{cmd, trace, Trace} ->
	    set_trace(Trace),
	    handle_cmd(Bs, Cm, Le, break, LineNo, F);
	{cmd, stack_trace, Flag} ->
	    set_stack_trace(Flag),
	    handle_cmd(Bs, Cm, Le, break, LineNo, F);
	{cmd, inspect, {From,Var,SP}} ->
	    inspect_var(From,Var,Bs,SP),
	    handle_cmd(Bs, Cm, Le, break, LineNo, F);
	{cmd, inspect_def, Info} ->
	    inspect_def(Info, Bs),
	    handle_cmd(Bs, Cm, Le, break, LineNo, F);
	{cmd, messages, {From}} ->
	    From ! {self(), messages, get_messages()},
	    handle_cmd(Bs, Cm, Le, break, LineNo, F);
	{cmd, stack_frame, {From,Dir,SP}} ->
	    From ! {self(), stack_frame, stack_frame(Dir,SP)},
	    handle_cmd(Bs, Cm, Le, break, LineNo, F);
	{cmd, backtrace, {From,N}} ->
	    From ! {self(), backtrace, backtrace1(N)},
	    handle_cmd(Bs, Cm, Le, break, LineNo, F);
	{cmd, command, {From,Mod,Cmd,CmdNo,SP}} ->
	    Bs1 = user_command(From, Mod, Cmd, Bs, Cm, Le, LineNo,
			       CmdNo, SP, F),
	    handle_cmd(Bs1, Cm, Le, break, LineNo, F);
	{cmd, get_bindings, {From, SP}} ->
	    return_bindings(From, Bs, SP),
	    handle_cmd(Bs, Cm, Le, break, LineNo, F);
	{cmd, _, _} ->
	    handle_cmd(Bs, Cm, Le, break, LineNo, F);

	%% Messages from 'interpret' about break points,
	{break_msg,Type,Data} ->
	    break_msg(Type,Data),
	    handle_cmd(Bs, Cm, Le, break, LineNo, F);

	%% Handle removal of interpreted code.
	{old_code, Module} ->

	    case dbg_ieval:in_use_p(Module,Cm) of
		%% A call to the Module is on the stack (or unknown), so we must terminate.
		true ->
%		    tell_attached({self(),old_code,Module}),
		    exit(get(self), kill),
		    dbg_ieval:exit(old_code, Cm, LineNo, Bs);
		false ->
		    erase([Module|db]),
		    erase(cache),		    
		    handle_cmd(Bs, Cm, Le, break, LineNo, F)
	    end;

	%%FIXME: Handle new_code.
	{new_code, Module} ->
	    handle_cmd(Bs, Cm, Le, break, LineNo, F);



	{'EXIT',Pid,Reason} ->

	    case attached_p(Pid) of
		true ->
		    detach(Pid),
		    handle_cmd(Bs, Cm, Le, break, LineNo, F);
		AttRet ->
		    case get(self) of
			Pid ->   % Msg_handler died !
			    dbg_ieval:exit(Pid,Reason,Cm,LineNo,Bs);
			_ ->
			    dbg_ieval:exit(Reason,Cm,LineNo,Bs)
		    end
	    

	    end;
	Otherwise ->
	    io:format("handle_cmd[1]: Unexpected unhandled message: ~p~n",[Otherwise]),
	    exit(unexpected_message)
    
    end;
handle_cmd(Bs, Cm, Le, State, LineNo, F) ->  % State = running or Next Level
    receive

	{attach,Msg_handler,AttPid} ->
	    attach(AttPid,Cm,LineNo),
	    handle_cmd(Bs, Cm, Le, State, LineNo, F);
	{cmd, break, {Mod, Line}} ->
	    new_break({Mod,Line}),
	    handle_cmd(Bs, Cm, Le, State, LineNo, F);
	{cmd, delete_break, {Mod, Line}} ->
	    delete_break({Mod,Line}),
	    handle_cmd(Bs, Cm, Le, State, LineNo, F);
	{cmd, no_break, Mod} ->
	    no_break1(Mod),
	    handle_cmd(Bs, Cm, Le, State, LineNo, F);
	{cmd, inspect, {From,Var,SP}} ->
	    inspect_var(From,Var,Bs,SP),
	    handle_cmd(Bs, Cm, Le, State, LineNo, F);
	{cmd, inspect_def, Info} ->
	    inspect_def(Info, Bs),
	    handle_cmd(Bs, Cm, Le, State, LineNo, F);
	{cmd, messages, {From}} ->                        %% TBD: should messages be here ??? 1/9-94
	    From ! {self(), messages, get_messages()},
	    handle_cmd(Bs, Cm, Le, State, LineNo, F);
	{cmd, stack_frame, {From,Dir,SP}} ->
	    From ! {self(), stack_frame, stack_frame(Dir,SP)},
	    handle_cmd(Bs, Cm, Le, State, LineNo, F);
	{cmd, backtrace, {From,N}} ->
	    From ! {self(), backtrace, backtrace1(N)},
	    handle_cmd(Bs, Cm, Le, State, LineNo, F);
	{cmd, trace, Trace} ->
	    set_trace(Trace),
	    handle_cmd(Bs, Cm, Le, State, LineNo, F);
	{cmd, stack_trace, Flag} ->
	    set_stack_trace(Flag),
	    handle_cmd(Bs, Cm, Le, State, LineNo, F);
	{cmd, command, {From,_,_,CmdNo,_}} ->
	    From ! {self(),command_resp,CmdNo,
		    {self(),'Commands not allowed while running'}},
	    handle_cmd(Bs, Cm, Le, State, LineNo, F);
	{cmd, get_bindings, {From, SP}} ->
	    return_bindings(From, Bs, SP),
	    handle_cmd(Bs, Cm, Le, State, LineNo, F);
	{cmd, _, _} ->
	    handle_cmd(Bs, Cm, Le, State, LineNo, F);

	%% Messages from 'interpret' about break points,
	{break_msg,Type,Data} ->
	    break_msg(Type,Data),
	    handle_cmd(Bs, Cm, Le, State, LineNo, F);

	%% Handle removal of interpreted code.
	{old_code, Module} ->
	    case dbg_ieval:in_use_p(Module,Cm) of
		%% A call to the Module is on the stack (or unknown), so we must terminate.
		true ->
%		    tell_attached({self(),old_code,Module}),
		    exit(get(self), kill),
		    dbg_ieval:exit(old_code, Cm, LineNo, Bs);
		false ->
		    erase([Module|db]),
		    erase(cache),
		    handle_cmd(Bs, Cm, Le, State, LineNo, F)
	    end;

	%%FIXME: Handle new_code.
	{new_code, Module} ->
	    io:format("NYI: Handling of {new_code,Module} in: handle_cmd,Module:~p~n",[Module]),
	    handle_cmd(Bs, Cm, Le, State, LineNo, F);




	{'EXIT',Pid,Reason} ->
	    case attached_p(Pid) of
		true ->
		    detach(Pid),
		    handle_cmd(Bs, Cm, Le, State, LineNo, F);
		_ ->
		    case get(self) of
			Pid ->   % Msg_handler died !
			    dbg_ieval:exit(Pid,Reason,Cm,LineNo,Bs);
			_ ->
			    dbg_ieval:exit(Reason,Cm,LineNo,Bs)
		    end
	    end;
	Otherwise ->
	    io:format("handle_cmd[2]: Unexpected unhandled message: ~p~n",[Otherwise]),
	    exit(unexpected_message)
	after 0 ->
	    Bs
    end.

user_command(From, Mod, Cmd, Bs, Cm, Le, LineNo, CmdNo, SP, F) ->
    case catch parse_cmd(Cmd, LineNo) of
	{'EXIT',_} ->
	    From ! {self(),command_resp,CmdNo,{self(),parse_error}},
	    Bs;
	parse_error ->
	    From ! {self(),command_resp,CmdNo,{self(),parse_error}},
	    Bs;
	Forms when SP < Le -> %% Evaluate in stack.
	    user_cmd_restricted(From, Forms, Bs, Mod, CmdNo, SP),
	    Bs;
	Forms ->
	    mark_running(LineNo, Le),
	    user_cmd(From, Forms, Mod, Bs, Cm, Le, LineNo, CmdNo, F)
    end.

user_command_restricted(From, Cmd, Bs, Mod, CmdNo, SP) ->
    case catch parse_cmd(Cmd, 1) of
	{'EXIT',_} ->
	    From ! {self(),command_resp,CmdNo,{self(),parse_error}},
	    Bs;
	parse_error ->
	    From ! {self(),command_resp,CmdNo,{self(),parse_error}},
	    Bs;
	Forms ->
	    user_cmd_restricted(From, Forms, Bs, Mod, CmdNo, SP)
    end.

parse_cmd([$?|Symbol0], LineNo) ->
    {ok,Tokens,Pos} = erl_scan:string(Symbol0, LineNo),
    case erl_parse:parse_exprs(Tokens) of
	{ok,[{atom,_,Symbol}]} ->
	    {def,Symbol};
	{ok,[{var,_,Symbol}]} ->
	    {def,Symbol};
	_ ->
	    parse_error
    end;
parse_cmd(Cmd, LineNo) ->
    {ok,Tokens,Pos} = erl_scan:string(Cmd, LineNo),
    case erl_parse:parse_exprs(Tokens) of
	{ok,Forms} ->
	    Forms;
	_ ->
	    parse_error
    end.

user_cmd_restricted(From, {def,Symbol}, Bs, Mod, CmdNo, _) -> % Expand macro
    case inspect_def1(Symbol,Mod,Bs) of
	{value,Value} ->
	    From ! {self(),command_resp,CmdNo,Value};
	Other ->
	    From ! {self(),command_resp,CmdNo,Other}
    end;
user_cmd_restricted(From, [{var,_,Var}], Bs, _, CmdNo, SP) ->
    case get_var(Var,Bs,SP) of
	{value,Value} -> Res = Value;
	Res           -> Res
    end,
    From ! {self(),command_resp,CmdNo,Res};
user_cmd_restricted(From, _, _, _, CmdNo,_) ->
    From ! {self(),command_resp,CmdNo,
	    {self(),'Only possible to inspect variables'}}.

user_cmd(From, {def,Symbol}, Mod, Bs, Cm, Le, LineNo, CmdNo, F) -> % Expand macro
    case inspect_def1(Symbol,Mod,Bs) of
	{value,Value} ->
	    From ! {self(),command_resp,CmdNo,Value},
	    mark_break(Cm,LineNo,Le),
	    Bs;
	Other ->
	    From ! {self(),command_resp,CmdNo,Other},
	    mark_break(Cm,LineNo,Le),
	    Bs
    end;
user_cmd(From, [{match,_,{var,_,Var},Expr}], _, Bs, Cm, Le, LineNo, CmdNo, F) ->
    {value,Value,Bs1} = dbg_ieval:eval_expr(Expr,Bs,Cm,false,Le,F),
    From ! {self(),command_resp,CmdNo,Value},
    case lists:keysearch(Var, 1, Bs) of
	{value, _} ->
	    Bs2 = lists:keyreplace(Var, 1, Bs1, {Var,Value}),
	    mark_break(Cm,LineNo,Le),
	    Bs2;
	false ->
	    mark_break(Cm,LineNo,Le),
	    [{Var,Value}|Bs1]
    end;
user_cmd(From, [{var,_,Var}], _, Bs, Cm, Le, LineNo, CmdNo, F) ->
    case lists:keysearch(Var, 1, Bs) of
	{value, {Var, Value}} ->
	    From ! {self(),command_resp,CmdNo,Value},
	    mark_break(Cm,LineNo,Le),
	    Bs;
	_ ->
	    From ! {self(),command_resp,CmdNo,unbound},
	    mark_break(Cm,LineNo,Le),
	    Bs
    end;
user_cmd(From, [Expr], _, Bs, Cm, Le, LineNo, CmdNo, F) ->
    {value,Value,Bs1} = dbg_ieval:eval_expr(Expr,Bs,Cm,false,Le,F),
    From ! {self(),command_resp,CmdNo,Value},
    mark_break(Cm,LineNo,Le),
    Bs1;
user_cmd(From, [{match,_,{var,_,Var},Expr}|Exprs],
	 M, Bs, Cm, Le, LineNo, CmdNo, F) ->
    {value,Value,Bs1} = dbg_ieval:eval_expr(Expr,Bs,Cm,false,Le,F),
    case lists:keysearch(Var, 1, Bs) of
	{value, _} ->
	    Bs2 = lists:keyreplace(Var, 1, Bs1, {Var,Value}),
	    user_cmd(From, Exprs, M, Bs2, Cm, Le, LineNo, CmdNo, F);
	false ->
	    user_cmd(From, Exprs, M, [{Var,Value}|Bs], Cm, Le, LineNo, CmdNo, F)
    end;
user_cmd(From, [{var,_,Var}|Exprs], M, Bs, Cm, Le, LineNo, CmdNo, F) ->
    case lists:keysearch(Var, 1, Bs) of
	{value, {Var, Value}} ->
	    user_cmd(From, Exprs, M, Bs, Cm, Le, LineNo, CmdNo, F);
	_ ->
	    user_cmd(From, Exprs, M, Bs, Cm, Le, LineNo, CmdNo, F)
    end;
user_cmd(From, [Expr|Exprs], M, Bs, Cm, Le, LineNo, CmdNo, F) ->
    {value,Value,Bs1} = dbg_ieval:eval_expr(Expr,Bs,Cm,false,Le,F),
    user_cmd(From, Exprs, M, Bs1, Cm, Le, LineNo, CmdNo, F).
    
mark_running(LineNo,Le) ->
    put(state,running),
    put(user_cmd,[{LineNo,Le}|get(user_cmd)]),
    dbg_iserver_api:set_state(running),
    tell_attached({self(),running}).

mark_break(Cm,LineNo,Le) ->
    put(state,break),
    put(user_cmd,tl(get(user_cmd))),
    tell_attached({self(),break_at,Cm,LineNo,Le}),
    dbg_iserver_api:break_at(Cm, LineNo, false).%% Already attached here !

attach(AttPid) ->     % Idle waiting for new call
    put(state,break),
    Attached = get(attached),
    case lists:member(AttPid,Attached) of
	false ->
	    int:attached(AttPid),
	    put(attached,[AttPid|Attached]);
	_ ->
	    true
    end,
    set_trace_flag(get(trace_f),true),
    AttPid ! {self(),attached,get(trace_f)}.

attach(AttPid,Cm,LineNo) ->
    put(state,break),
    Attached = get(attached),
    case lists:member(AttPid,Attached) of
	false ->
	    int:attached(AttPid),
	    put(attached,[AttPid|Attached]);
	_ ->
	    true
    end,
    set_trace_flag(get(trace_f),true),
    AttPid ! {self(),attached,Cm,LineNo,get(trace_f)}.

attached_p(Pid) ->
    case get(attached) of
	[] ->
	    false;
	Attached ->
	    lists:member(Pid,Attached)
    end.

detach(Pid) ->
    Attached = lists:delete(Pid,get(attached)),
    put(attached,Attached),
    case Attached of
	[] ->
	    set_trace_flag(get(trace_f),false);
	_ ->
	    true
    end.

tell_attached(Msg) -> tell_attached(get(attached),Msg).

tell_attached([],_) ->
    true;
tell_attached([AttPid|Atts],Msg) ->
    AttPid ! Msg,
    tell_attached(Atts,Msg).

%%%------------------------------------------------------
%%% Go up/down the stack.
%%% Return {SP',Module,Line}
%%%------------------------------------------------------

stack_frame(up,SP) -> stack_up_frame(SP-1);
stack_frame(_,SP)  -> stack_down_frame(SP+1).

stack_up_frame(SP0) ->
    case get_up_frame(SP0,dbg_ieval:get_stack()) of
	{SP,{Mod,_,Line,_}} ->
	    {SP,Mod,Line};
	What ->
	    What
    end.

get_up_frame(SP,[]) ->
    top;
get_up_frame(SP,[{Le,Frame}|S]) when SP < Le ->
    get_up_frame(SP,S);
get_up_frame(SP,[{Le,Frame}|_]) ->
    {Le,Frame}.

stack_down_frame(SP0) ->
    case get_down_frame(SP0,dbg_ieval:get_stack(),bottom) of
	{SP,{Mod,_,Line,_}} ->
	    {SP,Mod,Line};
	What ->
	    What
    end.

get_down_frame(SP,[{Le,Frame}|S],_) when SP < Le ->
    get_down_frame(SP,S,{Le,Frame});
get_down_frame(SP,[{SP,Frame}|_],_) ->
    {SP,Frame};
get_down_frame(SP,_,Above) ->
    Above.

%%-- Get the program stack.
%%-- Return a list of entrys (except the bindings).
%%--   [{CallLevel,Mod,Function,LineNo},...]
%%--    where Function == {Fnk,Arity} or extern.

backtrace1(N) -> backtrace1(N,dbg_ieval:get_stack()).

backtrace1(all,S) ->
    backtrace1(length(S),S);
backtrace1(N,[{Le,{M,F,L,_}}|S]) when N > 0 ->
    [{Le,M,F,L}|backtrace1(N-1,S)];
backtrace1(_,_) ->
    [].

%%%------------------------------------------------------
%%% Get the variable binding of Var from Bs.
%%% If SP in stack get the corresponding binding at
%%% that (SP) level.
%%% Return to the requesting process.
%%%------------------------------------------------------

inspect_var(From,Var,Bs,SP) ->
    Res = get_var(to_atom(Var),Bs,SP),
    From ! {self(),inspect_res,Res}.

get_var(Var,Bs0,SP) ->
    Bs = get_bs(Bs0,SP),
    get_binding(Var,Bs).
    
%%%------------------------------------------------------
%%% Get all variable bindings.
%%% If SP in stack get the corresponding bindings at
%%% that (SP) level.
%%% Return to the requesting process.
%%%------------------------------------------------------

return_bindings(From, Bs, SP) ->
    Res = get_bs(Bs, SP),
    From ! {self(), bindings, Res}.

get_bs(Bs, nostack) ->
    Bs;

get_bs(Bs, SP) ->
    get_bs(Bs, SP, dbg_ieval:get_stack()).

get_bs(Bs,SP,Stack) when SP > element(1,hd(Stack)) ->
    Bs;
get_bs(_,SP,Stack) ->
    get_bs1(SP,Stack).

get_bs1(SP,[{SP,{_,_,_,Bs}}|_])     -> Bs;
get_bs1(SP,[{Le,_}|_]) when SP > Le -> [];
get_bs1(SP,[_|S])                   -> get_bs1(SP,S);
get_bs1(_,[])                       -> [].

%%%------------------------------------------------------
%%% Get the macro definition of the symbol Def according
%%% to Mod.
%%% Return to the requesting process.
%%%------------------------------------------------------

inspect_def({From,Def,Mod},Bs) ->
    From ! {self(),inspect_def_res,inspect_def1(Def,Mod,Bs)}.
    
inspect_def1(Def0,Mod,Bs) ->
    Def = to_atom(Def0),
    case get_db(Mod) of
	undefined ->
	    symbol_unknown;
	DbRef ->
	    case dbg_idb:lookup(DbRef,defs) of
		not_found ->
		    symbol_unknown;
		{ok,Defs} when list(Defs) ->
		    case lists:keysearch(Def,1,Defs) of
			{value,{Def,String}} ->
			    {value,String};
			_ ->
			    symbol_unknown
		    end;
		_ ->
		    symbol_unknown
	    end
    end.

get_db(Mod) ->
    case get([Mod|db]) of
	undefined ->
	    case dbg_idb:which_db(get(self),Mod) of  % For exited proc.
		not_found ->
		    undefined;
		DbRef ->
		    DbRef
	    end;
	DbRef ->
	    DbRef
    end.

to_atom(Atom) when atom(Atom) -> Atom;
to_atom(List) when list(List) -> list_to_atom(List);
to_atom(What)                 -> What.

get_messages() ->
    {_,Msgs} = erlang:process_info(get(self),messages),
    Msgs.

%% EXPORT !!!!
break_msg(new_break_options,{Break,Options}) ->
    store_break_info(change,{Break,Options});
break_msg(no_break,Break) ->
    store_break_info(delete_all,Break);
break_msg(delete_break,Break) ->
    store_break_info(delete,Break);
break_msg(new_break,{Break,Options}) ->
    store_break_info(new,{Break,Options}).

%%%------------------------------------------------------
%%% Monitor break points in every interpreted process.
%%% Break points are stored in a structure as:
%%%    [{Mod1,[{Line1,Op},{Line2,Op},...]},{Mod2,[...]},...]
%%%------------------------------------------------------

init_breaks() ->  %% EXPORT !!
    put(break_points,init_breaks(dbg_idb:get_all_breaks(),[])).

init_breaks([{{Mod,Line},Op}|Brs],BrStr) ->
    init_breaks(Brs,store_new_break(Mod,Line,Op,BrStr));
init_breaks([],BrStr) ->
    BrStr.

store_break_info(new,{{Mod,Line},Options}) ->
    put(break_points,
	store_new_break(Mod,Line,Options,get(break_points)));
store_break_info(change,{{Mod,Line},Options}) ->
    put(break_points,
	store_changed_break(Mod,Line,Options,get(break_points)));
store_break_info(delete,{Mod,Line}) ->
    put(break_points,
	store_delete_break(Mod,Line,get(break_points)));
store_break_info(delete_all,{}) ->
    put(break_points,[]);
store_break_info(delete_all,{Mod}) ->
    put(break_points,
	store_deleteall_break(Mod,get(break_points))).

store_new_break(Mod,Line,Options,[{Mod,Ps}|Brs]) ->
    [{Mod,[{Line,Options}|Ps]}|Brs];
store_new_break(Mod,Line,Options,[B|Brs]) ->
    [B|store_new_break(Mod,Line,Options,Brs)];
store_new_break(Mod,Line,Options,[]) ->
    [{Mod,[{Line,Options}]}].

store_changed_break(Mod,Line,Options,[{Mod,Ps}|Brs]) ->
    Ps1 = store_changed_break1(Line,Options,Ps),
    [{Mod,Ps1}|Brs];
store_changed_break(Mod,Line,Options,[B|Brs]) ->
    [B|store_changed_break(Mod,Line,Options,Brs)];
store_changed_break(Mod,Line,Options,[]) ->
    []. % Can (should) never occur

store_changed_break1(Line,Options,[{Line,_}|Ps]) ->
    [{Line,Options}|Ps];
store_changed_break1(Line,Options,[B|Ps]) ->
    [B|store_changed_break1(Line,Options,Ps)];
store_changed_break1(Line,Options,[]) ->
    []. % Can (should) never occur

store_delete_break(Mod,Line,[{Mod,Ps}|Brs]) ->
    Ps1 = store_delete_break1(Line,Ps),
    [{Mod,Ps1}|Brs];
store_delete_break(Mod,Line,[B|Brs]) ->
    [B|store_delete_break(Mod,Line,Brs)];
store_delete_break(Mod,Line,[]) ->
    []. % Can (should) never occur

store_delete_break1(Line,[{Line,_}|Ps]) ->
    Ps;
store_delete_break1(Line,[B|Ps]) ->
    [B|store_delete_break1(Line,Ps)];
store_delete_break1(Line,[]) ->
    []. % Can (should) never occur

store_deleteall_break(Mod,[{Mod,_}|Brs]) ->
    Brs;
store_deleteall_break(Mod,[B|Brs]) ->
    [B|store_deleteall_break(Mod,Brs)];
store_deleteall_break(_,[]) ->
    [].

%%%------------------------------------------------------
%%% Command handling in interpreted 'receive'
%%%------------------------------------------------------

cmd_rec(send, {Msg},_) ->
    get(self) ! Msg;
cmd_rec(timeout, _,_) ->
    true; % Handled in do_receive
cmd_rec(break, {Mod, Line},_) ->
    new_break({Mod, Line});
cmd_rec(delete_break, {Mod, Line},_) ->
    delete_break({Mod, Line});
cmd_rec(no_break, Mod,_) ->
    no_break1(Mod);
cmd_rec(inspect,{From,Var,SP},Bs) ->
    inspect_var(From,Var,Bs,SP);
cmd_rec(inspect_def,Info,Bs) ->
    inspect_def(Info,Bs);
cmd_rec(command,{From,Mod,Cmd,CmdNo,SP},Bs) ->
    user_command_restricted(From, Cmd, Bs, Mod, CmdNo, SP);
cmd_rec(messages,{From},Bs) ->
    From ! {self(), messages, get_messages()};
cmd_rec(stack_frame,{From,Dir,SP},_) ->
    From ! {self(), stack_frame, stack_frame(Dir,SP)};
cmd_rec(backtrace,{From,N},_) ->
    From ! {self(), backtrace, backtrace1(N)};
cmd_rec(trace,Trace,Bs) ->
    set_trace(Trace);
cmd_rec(stack_trace,Flag,Bs) ->
    set_stack_trace(Flag);
cmd_rec(get_bindings,{From,SP},Bs) ->
    return_bindings(From, Bs, SP);
cmd_rec(_, _,_) ->
    true.


%%%------------------------------------------------------
%%% Command handling while waiting for new function to
%%% interpret.
%%%------------------------------------------------------

idle_wait({cmd, break, {Mod, Line}}) ->
    new_break({Mod,Line});
idle_wait({cmd, delete_break, {Mod, Line}}) ->
    delete_break({Mod, Line});
idle_wait({cmd, no_break, Mod}) ->
    no_break1(Mod);
idle_wait({cmd, send, {Msg}}) ->
    get(self) ! Msg;
idle_wait({cmd, trace, Trace}) ->
    set_trace(Trace);
idle_wait({cmd, stack_trace, Flag}) ->
    set_stack_trace(Flag);
idle_wait({cmd, stack_frame, {From,Dir,SP}}) ->
    From ! {self(), stack_frame, stack_frame(Dir,SP)},
    true;
idle_wait({cmd, backtrace, {From,N}}) ->
    From ! {self(), backtrace, backtrace1(N)},
    true;
idle_wait({cmd, get_bindings, {From, SP}}) ->
    return_bindings(From, [], SP);
idle_wait(_) ->
    true.

%%%------------------------------------------------------
%%% Command handling while waiting for external function
%%% to return.
%%%------------------------------------------------------

wait_cmd({cmd, break, {Mod, Line}},_) ->
    new_break({Mod,Line});
wait_cmd({cmd, delete_break, {Mod, Line}},_) ->
    delete_break({Mod, Line});
wait_cmd({cmd, no_break, Mod},_) ->
    no_break1(Mod);
wait_cmd({cmd, send, {Msg}},_) ->
    get(self) ! Msg;
wait_cmd({cmd,inspect,{From,Var,SP}},Bs) ->
    inspect_var(From,Var,Bs,SP);
wait_cmd({cmd,inspect_def,Info},Bs) ->
    inspect_def(Info,Bs);
wait_cmd({cmd,command,{From,Mod,Cmd,CmdNo,SP}},Bs) ->
    user_command_restricted(From, Cmd, Bs, Mod, CmdNo, SP);
wait_cmd({cmd, trace, Trace},Bs) ->
    set_trace(Trace);
wait_cmd({cmd, stack_trace, Flag},Bs) ->
    set_stack_trace(Flag);
wait_cmd({cmd, stack_frame, {From,Dir,SP}},_) ->
    From ! {self(), stack_frame, stack_frame(Dir,SP)},
    true;
wait_cmd({cmd, backtrace, {From,N}},_) ->
    From ! {self(), backtrace, backtrace1(N)},
    true;
wait_cmd({cmd, get_bindings, {From, SP}},Bs) ->
    return_bindings(From, Bs, SP);
wait_cmd(_,_) ->
    true.

%%%------------------------------------------------------
%%% Command handling for a terminated process.
%%%------------------------------------------------------

exit_cmd({cmd,inspect,{From,Var,SP}},Bs) ->
    inspect_var(From,Var,Bs,SP);
exit_cmd({cmd,inspect_def,Info},Bs) ->
    inspect_def(Info,Bs);
exit_cmd({cmd,command,{From,Mod,Cmd,CmdNo,SP}},Bs) ->
    user_command_restricted(From, Cmd, Bs, Mod, CmdNo, SP);
exit_cmd({cmd,stack_frame,{From,Dir,SP}},_) ->
    From ! {self(), stack_frame, stack_frame(Dir,SP)},
    true;
exit_cmd({cmd,backtrace,{From,N}},_) ->
    From ! {self(), backtrace, backtrace1(N)},
    true;
exit_cmd({cmd, get_bindings, {From, SP}},Bs) ->
    return_bindings(From, Bs, SP);
exit_cmd(_,_) ->
    true.

%%---------------------------------------------------------------
%% Check if where is a break point at line 'Line' in module 'Mod'
%%---------------------------------------------------------------

break_p(Mod, Line, Le, Bs) ->
    case break_p1(Mod, Line, get(break_points)) of
	{ok, [active|Opts]} ->
	    case user_cmd_line_level_p(get(user_cmd),Line,Le) of
		true ->
		    false;
		_ ->
		    break_now_p([active|Opts],Mod,Line,Bs)
	    end;
	_ ->
	    false
    end.

user_cmd_line_level_p([{Line,Le}|_],Line,Le) -> true;
user_cmd_line_level_p(_,_,_)                 -> false.

break_now_p([active,Action,_,CondFnk], Mod, Line, Bs) ->
    case conditional_p(CondFnk,Bs) of
	true ->
	    case Action of
		disable ->
		    disable_break(Mod,Line),
		    true;
		delete ->
		    delete_break(Mod,Line),
		    true;
		_ ->
		    true
	    end;
	_ ->
	    false
    end.

conditional_p({Mod,Fnk}, Bs)      -> catch apply(Mod,Fnk,[Bs]);
conditional_p({Mod,Fnk,Args}, Bs) -> catch apply(Mod,Fnk,[Bs|Args]);
conditional_p(_, _)               -> true.  % Not conditional

break_p1(_,_,[])                 -> false;
break_p1(M,L,[{M,Ps}|_])         -> break_p2(L,Ps);
break_p1(M,L,[_,{M,Ps}|_])       -> break_p2(L,Ps);
break_p1(M,L,[_,_,{M,Ps}|_])     -> break_p2(L,Ps);
break_p1(M,L,[_,_,_,{M,Ps}|_])   -> break_p2(L,Ps);
break_p1(M,L,[_,_,_,_,{M,Ps}|_]) -> break_p2(L,Ps);
break_p1(M,L,[_,_,_,_,_|Brs])    -> break_p1(M,L,Brs);
break_p1(M,L,[_,_,_,_|Brs])      -> break_p1(M,L,Brs);
break_p1(M,L,[_,_,_|Brs])        -> break_p1(M,L,Brs);
break_p1(M,L,[_,_|Brs])          -> break_p1(M,L,Brs);
break_p1(M,L,[_|Brs])            -> break_p1(M,L,Brs).

break_p2(L,[])                 -> false;
break_p2(L,[{L,Op}|_])         -> {ok,Op};
break_p2(L,[_,{L,Op}|_])       -> {ok,Op};
break_p2(L,[_,_,{L,Op}|_])     -> {ok,Op};
break_p2(L,[_,_,_,{L,Op}|_])   -> {ok,Op};
break_p2(L,[_,_,_,_,{L,Op}|_]) -> {ok,Op};
break_p2(L,[_,_,_,_,_|Brs])    -> break_p2(L,Brs);
break_p2(L,[_,_,_,_|Brs])      -> break_p2(L,Brs);
break_p2(L,[_,_,_|Brs])        -> break_p2(L,Brs);
break_p2(L,[_,_|Brs])          -> break_p2(L,Brs);
break_p2(L,[_|Brs])            -> break_p2(L,Brs).

set_trace(true) ->
    put(trace_f,true),
    set_trace_flag(true,get(attached));
set_trace(false) ->
    put(trace_f,false),
    set_trace_flag(false,get(attached)).

%% -- Both trace_f and attached pids must be active to
%% -- activate trace.

set_trace_flag(_,[]) ->
    put(trace,false);
set_trace_flag(true,Attached) when list(Attached) ->
    tell_attached(Attached, {self(), trace_flag, true}),
    put(trace,true);
set_trace_flag(true,true) ->
    put(trace,true);
set_trace_flag(_,Attached) when list(Attached) ->
    tell_attached(Attached, {self(), trace_flag, false}),
    put(trace,false);
set_trace_flag(_,_) ->
    put(trace,false).

set_stack_trace(Flag) ->
    mark_stack_trace(Flag),
    tell_attached({self(), stack_trace_flag, Flag}),
    true.

mark_stack_trace(all)     -> put(stack_trace,all);
mark_stack_trace(true)    -> put(stack_trace,all);
mark_stack_trace(no_tail) -> put(stack_trace,no_tail);
mark_stack_trace(_)       ->
    erase(stack),
    put(stack_trace,false).
    
%%---------------------------------------------------------------
%% Internal functions handling user requests (User interface).
%%---------------------------------------------------------------

new_break(Break) ->
    case dbg_idb:lookup(Break) of
	{ok,_} ->
	    {error, break_exists};
	_ ->
	    new_b(Break,is_alive()),
	    ok
    end.

new_break(Break,Cond) ->
    case dbg_idb:lookup(Break) of
	{ok,_} ->
	    {error, break_exists};
	_ ->
	    new_b(Break,Cond,is_alive()),
	    ok
    end.

new_b(Break,false) ->
    Options = [active,enable,all,null],
    new_break1(Break,Options);
new_b(Break,true) ->
    Options = [active,enable,all,null],
    dbg_idb:insert(Break,Options),
    rpc:eval_everywhere(?MODULE,new_break1,[Break,Options]).

new_b(Break,Cond,false) ->
    Options = [active,enable,all,Cond],
    new_break1(Break,Options);
new_b(Break,Cond,true) ->
    Options = [active,enable,all,Cond],
    rpc:eval_everywhere(?MODULE,new_break1,[Break,Options]).

new_break1({Mod,Line},Options) ->
    dbg_iserver_api:new_break(Mod, Line, Options),
    dbg_idb:insert({Mod,Line},Options).

break_in1(Mod,Fnk,Arity) ->
    case dbg_idb:lookup(Mod, Fnk, Arity) of
	{ok,Cs} ->
	    break_all_clauses(Mod,Cs,null);
	_ ->
	    {error,function_not_found}
    end.

break_in1(Mod,Fnk,Arity,Cond) ->
    case cond_p(Cond) of
	true ->
	    case dbg_idb:lookup(Mod, Fnk, Arity) of
		{ok,Cs} ->
		    break_all_clauses(Mod,Cs,Cond);
		_ ->
		    {error,function_not_found}
	    end;
	_ ->
	    {error,bad_condition}
    end.

cond_p({M,F}) when atom(M), atom(F) ->
    true;
cond_p({M,F,A}) when atom(M), atom(F), list(A) ->
    true;
cond_p(_) ->
    false.

break_all_clauses(Mod,[Clause|Cs],null) ->
    LineNo = element(2,hd(element(5,Clause))),
    break(Mod,LineNo),
    break_all_clauses(Mod,Cs,null);
break_all_clauses(Mod,[Clause|Cs],Cond) ->
    LineNo = element(2,hd(element(5,Clause))),
    break(Mod,LineNo,Cond),
    break_all_clauses(Mod,Cs,Cond);
break_all_clauses(_,[],_) ->
    ok.

del_break_in1(Mod,Fnk,Arity) ->
    case dbg_idb:lookup(Mod,{Fnk,Arity}) of
	{ok,Cs} ->
	    del_break_all_clauses(Mod,Cs);
	_ ->
	    {error,function_not_found}
    end.

del_break_all_clauses(Mod,[Clause|Cs]) ->
    LineNo = element(2,hd(element(5,Clause))),
    delete_break(Mod,LineNo),
    del_break_all_clauses(Mod,Cs);
del_break_all_clauses(_,[]) ->
    ok.

delete_break(Break) ->
    case dbg_idb:lookup(Break) of
	{ok,_} ->
	    delete_break1(Break,is_alive()),
	    ok;
	_ ->
	    {error, no_break_exists}
    end.

delete_break1(Break,false) ->
    delete_break1(Break);
delete_break1(Break,true) ->
    rpc:eval_everywhere(?MODULE,delete_break1,[Break]).

delete_break1({Mod, Line}) ->
    dbg_iserver_api:delete_break(Mod, Line),
    dbg_idb:delete({Mod,Line}).

no_break1(Break) ->
    no_break1(Break,is_alive()),
    ok.

no_break1(Break,false) ->
    no_break2(Break);
no_break1(Break,true) ->
    rpc:eval_everywhere(?MODULE,no_break2,[Break]).

no_break2({}) ->
    dbg_iserver_api:no_break(),
    dbg_idb:delete_all_breaks();
no_break2({Mod}) ->
    dbg_iserver_api:no_break(Mod),
    dbg_idb:delete_all_breaks(Mod).

trace_1(Trace) ->
    trace_1(Trace,is_alive()),
    ok.

trace_1(Trace,false) ->
    dbg_iserver_api:trace(Trace);
trace_1(Trace,true) ->
    rpc:eval_everywhere(dbg_iserver_api,trace,[Trace]).

stack_trace_1(Flag) ->
    stack_trace_1(Flag,is_alive()),
    ok.

stack_trace_1(Flag,false) ->
    dbg_iserver_api:stack_trace(Flag);
stack_trace_1(Flag,true) ->
    rpc:eval_everywhere(dbg_iserver_api,stack_trace,[Flag]).

disable_break1(Mod,Line) -> 
    case dbg_idb:lookup({Mod,Line}) of
	{ok, [active|Rest]} ->
	    update_break(Mod,Line,[inactive|Rest],is_alive());
	{ok, _} ->
	    ok;
	_ ->
	    {error,no_break}
    end.

enable_break1(Mod,Line) ->
    case dbg_idb:lookup({Mod,Line}) of
	{ok, [inactive|Rest]} ->
	    update_break(Mod,Line,[active|Rest],is_alive());
	{ok, _} ->
	    ok;
	_ ->
	    {error,no_break}
    end.

action_at_break1(Mod,Line,Action) ->
    case dbg_idb:lookup({Mod,Line}) of
	{ok, [Status,_|Rest]} ->
	    update_break(Mod,Line,[Status,Action|Rest],is_alive());
	_ ->
	    {error,no_break}
    end.

test_at_break1(Mod,Line,{M,F}) ->
    case dbg_idb:lookup({Mod,Line}) of
	{ok, [Status,Action,Procs,_|Rest]} ->
	    update_break(Mod,Line,[Status,Action,Procs,{M,F}|Rest],is_alive());
	_ ->
	    {error,no_break}
    end;
test_at_break1(Mod,Line,{M,F,Args}) when list(Args) ->
    case dbg_idb:lookup({Mod,Line}) of
	{ok, [Status,Action,Procs,_|Rest]} ->
	    update_break(Mod,Line,[Status,Action,Procs,{M,F,Args}|Rest],
			 is_alive());
	_ ->
	    {error,no_break}
    end;
test_at_break1(Mod,Line,Fnk) ->
    {error,{badarg,Fnk}}.

update_break(Mod,Line,NewOptions,false) ->
    update_break(Mod,Line,NewOptions);
update_break(Mod,Line,NewOptions,_) ->
    rpc:eval_everywhere(?MODULE,update_break,[Mod,Line,NewOptions]),
    ok.
    
update_break(Mod,Line,NewOptions) ->
    dbg_iserver_api:new_break_options(Mod, Line, NewOptions),
    dbg_idb:insert({Mod,Line},NewOptions).





