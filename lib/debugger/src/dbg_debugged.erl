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
%%

%%
%% This module contains the message loops for the process being debugged
%% and the main entry point from the break pointer handler in
%% the error_handler module (via the int module).
%%
%% When a process is debugged, most code is executed in another process,
%% called the meta process. When the meta process is interpreting code,
%% the process being debugged just waits in a receive loop (in this
%% module).
%%
%% This module also calls any BIFs that must execute in the correct
%% process (such as link/1 and spawn_link/1), and external code which
%% is not being interpreted.
%% 

-module(dbg_debugged).

-export([eval/3]).

-export([follow/4]).

%% eval(Mod, Func, Args) -> Value
%%  Main entry point from external (non-interpreted) code.
%%  We are called via the error handler when a breakpoint is hit.

eval(Mod, Func, Args) ->
    Meta = dbg_imeta:eval(Mod, Func, Args),
    msg(Meta).

msg(Meta) ->
    case catch msg_loop(Meta) of
	{ready,Meta,Value} ->
	    Value;
	{'EXIT',Reason} ->
	    Meta ! {sys,self(),{exited_nocatch,Reason}},
	    wait_exit(Meta);
	Thrown ->
	    Meta ! {sys,self(),{thrown_nocatch,Thrown}},
	    throw(Thrown)
    end.

msg_loop(Meta) ->
    receive
	{sys,Meta,Command} ->
	    handle_command(Meta, Command);
	{'EXIT',Meta,Reason} ->
	    exit(Reason)
    end.

handle_command(Meta, {ready,Val}) ->
    {ready,Meta,Val};
handle_command(Meta, {'receive',Msg}) ->
    receive
	Msg -> 
	    Meta ! {self(),rec_acked}
    end,
    msg_loop(Meta);
handle_command(Meta, {exit,Reason}) ->
    exit(Reason);
handle_command(Meta, {bif,Module,Name,As,Where,Followed}) ->
    Res = bif(Module, Name, As, Followed, Where),
    Meta ! {sys,self(),{apply_result,Res}},
    msg_loop(Meta);
handle_command(Meta, {catch_bif,Module,Name,As,Where,Followed}) ->
    send_result(Meta, catch catch_bif(Meta, Module, Name, As, Followed, Where)),
    msg_loop(Meta);
handle_command(Meta, {apply,Mod,Fnk,As}) ->
    Res = apply(Mod, Fnk, As),
    Meta ! {sys,self(),{apply_result,Res}},
    msg_loop(Meta);
handle_command(Meta, {catch_apply,Mod,Fnk,As}) ->
    send_result(Meta, catch catch_apply(Meta, Mod, Fnk, As)),
    msg_loop(Meta);
handle_command(Meta, {eval,Expr,Bs0}) ->
    Ref = make_ref(),
    case catch {Ref,erl_eval:expr(Expr, Bs0)} of
	{Ref,{value,V,Bs}} ->
	    Meta ! {sys,self(),{eval_result,V,Bs}};
	Other ->
	    Meta ! {sys,self(),{thrown,Other}}
    end,
    msg_loop(Meta);
handle_command(Meta, Command) ->
    io:format("~p:handle_command/1: ignoring ~p\n", [?MODULE,Command]),
    msg_loop(Meta).

send_result(Meta, {catch_normal,Meta,Res}) ->
    Meta ! {sys,self(),{apply_result,Res}};
send_result(Meta, Thrown) ->
    Meta ! {sys,self(),{thrown,Thrown}}.

%%-- Return tuple if apply evaluates normally, otherwise the
%%-- surrounding catch notices the unnormal exit.

catch_apply(Meta, Mod, Fnk, As) ->
    Res = apply(Mod, Fnk, As),
    {catch_normal,Meta,Res}.

catch_bif(Meta, Module, Name, As, Followed, Where) ->
    Res = bif(Module, Name, As, Followed, Where),
    {catch_normal,Meta,Res}.

%% bif(Module, Name, Arguments)
%%  Evaluate a BIF.

bif(Module, Name, As, false, Where) ->
    erts_debug:apply(Module, Name, As, Where);
bif(erlang, spawn, [M,F,As], Attached, Where) ->
    spawn(?MODULE,follow,[Attached,M,F,As]);
bif(erlang, spawn_link, [M,F,As], Attached, Where) ->
    spawn_link(?MODULE,follow,[Attached,M,F,As]);
bif(erlang, spawn, [N,M,F,As], Attached, Where) ->
    spawn(N,?MODULE,follow,[Attached,M,F,As]);
bif(erlang, spawn_link, [N,M,F,As], Attached, Where) ->
    spawn_link(N,?MODULE,follow,[Attached,M,F,As]).

follow(Fol,M,F,As) ->
    dbg_iserver_api:am_followed(Fol),
    apply(M, F, As).

%%---------------------------------------------------
%%-- Sync on exit.
%%-- The Meta process shall initiate all exits!
%%---------------------------------------------------

wait_exit(Meta) ->
    receive
	{sys,Meta,{exit,Reason}} ->
	    exit(Reason);
	{'EXIT',Meta,Reason} ->
	    exit(Reason)
    end.
