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
-module(dbg_debugged).

%% External exports
-export([eval/3]).

%% Internal exports
-export([follow/4]).

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
%% eval(Mod, Func, Args) -> Value
%% Main entry point from external (non-interpreted) code.
%% Called via the error handler when a breakpoint is hit.
%%--------------------------------------------------------------------
eval(Mod, Func, Args) ->
    Meta = dbg_imeta:eval(Mod, Func, Args),
    msg(Meta).

%%====================================================================
%% Internal exports
%%====================================================================

follow(_Fol, M, F, As) ->
    apply(M, F, As).


%%====================================================================
%% Internal functions
%%====================================================================

msg(Meta) ->
    case catch msg_loop(Meta) of
	{ready,Meta,Value} ->
	    ?DBG("Result~n",[]),
	    Value;
	{'EXIT',Reason0} ->
	    ?DBG("EXIT ~p~n",[Reason0]),
	    Reason = remove_debugger_calls(Reason0),
	    Meta ! {sys,self(),{exited_nocatch,Reason}},
	    wait_exit(Meta);
	{'EXIT',Meta,Reason} ->
	    ?DBG("EXIT ~p~n",[Reason]),
	    exit(Reason);
	Thrown ->
	    ?DBG("Thrown ~p~n",[Thrown]),
	    Meta ! {sys,self(),{thrown_nocatch,Thrown}},
	    throw(Thrown)
    end.

msg_loop(Meta) ->
    receive
	{sys,Meta,Command} ->
	    ?DBG("Command~p~n",[Command]),
	    handle_command(Meta, Command);
	{'EXIT',Meta,Reason} ->
	    {'EXIT',Meta,Reason}
    end.

handle_command(Meta, {ready,Val}) ->
    {ready,Meta,Val};
handle_command(Meta, {'receive',Msg}) ->
    receive
	Msg -> 
	    Meta ! {self(),rec_acked}
    end,
    msg_loop(Meta);
handle_command(_Meta, {exit,Reason}) ->
    exit(Reason);
handle_command(Meta, {bif,Mod,Name,As,Where,Followed}) ->
    Res = bif(Mod, Name, As, Followed, Where),
    Meta ! {sys,self(),{apply_result,Res}},
    msg_loop(Meta);
handle_command(Meta, {catch_bif,Mod,Name,As,Where,Followed}) ->
    send_result(Meta, catch_bif(Meta, Mod, Name, As, Followed, Where)),
    msg_loop(Meta);
handle_command(Meta, {apply,Mod,Fnk,As}) ->
    Res = apply(Mod, Fnk, As),
    Meta ! {sys,self(),{apply_result,Res}},
    msg_loop(Meta);
handle_command(Meta, {catch_apply,Mod,Fnk,As}) ->
    send_result(Meta, catch_apply(Meta, Mod, Fnk, As)),
    msg_loop(Meta);
handle_command(Meta, {eval,Expr,Bs0}) ->
    Ref = make_ref(),
    case catch {Ref,erl_eval:expr(Expr, Bs0)} of
	{Ref,{value,V,Bs}} ->
	    Meta ! {sys,self(),{eval_result,V,Bs}};
	Other ->
	    Meta ! {sys,self(),{thrown,Other}}
    end,
    msg_loop(Meta).

send_result(Meta, {catch_normal,Meta,Res}) ->
    Meta ! {sys,self(),{apply_result,Res}};
send_result(Meta, Thrown) ->
    Meta ! {sys,self(),{thrown,Thrown}}.

%%-- Return tuple if apply evaluates normally, otherwise the
%%-- surrounding catch notices the unnormal exit.

catch_apply(Meta, Mod, Fnk, As) ->
    Ref = make_ref(),
    Res0 = (catch {Ref, apply(Mod, Fnk, As)}),
    case Res0 of
	{'EXIT', Reason} -> {'EXIT', remove_debugger_calls(Reason)};
	{Ref, Res} -> 
	    {catch_normal,Meta,Res};
	_ ->
	    Res0
    end.

catch_bif(Meta, Mod, Name, As, Followed, Where) ->
    Res = (catch bif(Mod, Name, As, Followed, Where)),
    case Res of
	{'EXIT', Reason} -> {'EXIT', remove_debugger_calls(Reason)};
	_ ->
	    {catch_normal,Meta,Res}
    end.

remove_debugger_calls({Reason,BT}) when is_list(BT) ->
    {Reason, remove_debugger_calls(BT, [])};
remove_debugger_calls(Reason) ->
    Reason.
remove_debugger_calls([{?MODULE, _F, _A}|R], Acc) ->
    remove_debugger_calls(R,Acc);
remove_debugger_calls([Other|R], Acc) ->
    remove_debugger_calls(R, [Other|Acc]);
remove_debugger_calls([],Acc) -> 
    lists:reverse(Acc);
remove_debugger_calls(What, Acc) ->
    lists:reverse([What|Acc]).

%% bif(Mod, Name, Arguments)
%%  Evaluate a BIF.

bif(Mod, Name, As, false, Where) ->
    erts_debug:apply(Mod, Name, As, Where);
bif(erlang, spawn, [M,F,As], Attached, _Where) ->
    spawn(?MODULE,follow,[Attached,M,F,As]);
bif(erlang, spawn_link, [M,F,As], Attached, _Where) ->
    spawn_link(?MODULE,follow,[Attached,M,F,As]);
bif(erlang, spawn, [N,M,F,As], Attached, _Where) ->
    spawn(N,?MODULE,follow,[Attached,M,F,As]);
bif(erlang, spawn_link, [N,M,F,As], Attached, _Where) ->
    spawn_link(N,?MODULE,follow,[Attached,M,F,As]).

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
