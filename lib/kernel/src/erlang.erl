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
-module(erlang).

-export([apply/2,apply/3,spawn/4,spawn_link/4,
	 spawn_opt/2,spawn_opt/3,spawn_opt/4,spawn_opt/5,
	 disconnect_node/1]).
-export([spawn/1, spawn_link/1, spawn/2, spawn_link/2]).
-export([yield/0]).
-export([crasher/6]).
-export([fun_info/1]).
-export([send_nosuspend/2, send_nosuspend/3]).
-export([localtime_to_universaltime/1]).

-export([dlink/1, dunlink/1, dsend/2, dsend/3, dgroup_leader/2,
	 dexit/2, dmonitor_node/2, dmonitor_p/2]).

-export([set_cookie/2, get_cookie/0]).

-export([nodes/0]).

-export([concat_binary/1,info/1]).

-export([memory/0, memory/1]).

-export([list_to_integer/2,integer_to_list/2]).

-deprecated([{old_binary_to_term,1},{info,1},{hash,2}]).

apply(Fun, Args) ->
    apply(Fun, Args).

apply(Mod, Name, Args) ->
    apply(Mod, Name, Args).


% Spawns with a fun
spawn(F) when function(F) ->
    spawn(erlang, apply, [F, []]);
spawn({M,F}=MF) when atom(M), atom(F) ->
    spawn(erlang, apply, [MF, []]);
spawn(F) ->
    erlang:fault(badarg, [F]).

spawn(N, F) when N == node() ->
    spawn(F);
spawn(N, F) when function(F) ->
    spawn(N, erlang, apply, [F, []]);
spawn(N, {M,F}=MF) when atom(M), atom(F) ->
    spawn(N, erlang, apply, [MF, []]);
spawn(N, F) ->
    erlang:fault(badarg, [N, F]).

spawn_link(F) when function(F) ->
    spawn_link(erlang, apply, [F, []]);
spawn_link({M,F}=MF) when atom(M), atom(F) ->
    spawn_link(erlang, apply, [MF, []]);
spawn_link(F) ->
    erlang:fault(badarg, [F]).

spawn_link(N, F) when N == node() ->
    spawn_link(F);
spawn_link(N, F) when function(F) ->
    spawn_link(N, erlang, apply, [F, []]);
spawn_link(N, {M,F}=MF) when atom(M), atom(F) ->
    spawn_link(N, erlang, apply, [MF, []]);
spawn_link(N, F) ->
    erlang:fault(badarg, [N, F]).

spawn_opt(F, O) when function(F) ->
    spawn_opt(erlang, apply, [F, []], O);
spawn_opt({M,F}=MF, O) when atom(M), atom(F) ->
    spawn_opt(erlang, apply, [MF, []], O);
spawn_opt({M,F,A}, O) -> % For (undocumented) backward compatibility
    spawn_opt(M, F, A, O);
spawn_opt(F, O) ->
    erlang:fault(badarg, [F, O]).

spawn_opt(N, F, O) when N == node() ->
    spawn_opt(F, O);
spawn_opt(N, F, O) when function(F) ->
    spawn_opt(N, erlang, apply, [F, []], O);
spawn_opt(N, {M,F}=MF, O) when atom(M), atom(F) ->
    spawn_opt(N, erlang, apply, [MF, []], O);
spawn_opt(N, F, O) ->
    erlang:fault(badarg, [N, F, O]).

% Spawns with MFA

spawn(N,M,F,A) when N == node(), atom(M), atom(F), list(A) ->
    spawn(M,F,A);
spawn(N,M,F,A) when atom(N), atom(M), atom(F) ->
    case is_well_formed_list(A) of
	true ->
	    ok;
	false ->
	    erlang:fault(badarg, [N, M, F, A])
    end,
    case catch gen_server:call({net_kernel,N},
			       {spawn,M,F,A,group_leader()},
			       infinity) of
	Pid when pid(Pid) ->
	    Pid;
	Error ->
	    case remote_spawn_error(Error, {no_link, N, M, F, A, []}) of
		{fault, Fault} ->
		    erlang:fault(Fault, [N, M, F, A]);
		Pid ->
		    Pid
	    end
    end;
spawn(N,M,F,A) ->
    erlang:fault(badarg, [N, M, F, A]).

spawn_link(N,M,F,A) when N == node(), atom(M), atom(F), list(A) ->
    spawn_link(M,F,A);
spawn_link(N,M,F,A) when atom(N), atom(M), atom(F) ->
    case is_well_formed_list(A) of
	true ->
	    ok;
	_ ->
	    erlang:fault(badarg, [N, M, F, A])
    end,
    case catch gen_server:call({net_kernel,N},
			       {spawn_link,M,F,A,group_leader()},
			       infinity) of
	Pid when pid(Pid) ->
	    Pid;
	Error ->
	    case remote_spawn_error(Error, {link, N, M, F, A, []}) of
		{fault, Fault} ->
		    erlang:fault(Fault, [N, M, F, A]);
		Pid ->
		    Pid
	    end
    end;
spawn_link(N,M,F,A) ->
    erlang:fault(badarg, [N, M, F, A]).

spawn_opt(M, F, A, Opts) ->
    case catch erlang:spawn_opt({M, F, A, Opts}) of
	Pid when pid(Pid) ->
	    Pid;
	{'EXIT', {Reason, _}} ->
	    erlang:fault(Reason, [M,F,A,Opts])
    end.

spawn_opt(N,M,F,A,O) when N == node(), atom(M), atom(F), list(A), list(O) ->
    spawn_opt(M,F,A,O);
spawn_opt(N,M,F,A,O) when atom(N), atom(M), atom(F) ->
    case {is_well_formed_list(A), is_well_formed_list(O)} of
	{true, true} ->
	    ok;
	_ ->
	    erlang:fault(badarg, [N, M, F, A, O])
    end,
    {L,NO} = lists:foldl(fun (link, {_, NewOpts}) ->
				 {link, NewOpts};
			     (Opt, {LO, NewOpts}) ->
				 {LO, [Opt|NewOpts]}
			 end,
			 {no_link,[]},
			 O),
    case catch gen_server:call({net_kernel,N},
			       {spawn_opt,M,F,A,NO,L,group_leader()},
			       infinity) of
	Pid when pid(Pid) ->
	    Pid;
	Error ->
	    case remote_spawn_error(Error, {L, N, M, F, A, NO}) of
		{fault, Fault} ->
		    erlang:fault(Fault, [N, M, F, A, O]);
		Pid ->
		    Pid
	    end
    end;
spawn_opt(N,M,F,A,O) ->
    erlang:fault(badarg, [N,M,F,A,O]).

remote_spawn_error({'EXIT', {{nodedown,N}, _}}, {L, N, M, F, A, O}) ->
    {Opts, LL} = case L == link of
		     true ->
			 {[link|O], [link]};
		     false ->
			 {O, []}
		 end,
    spawn_opt(erlang,crasher,[N,M,F,A,Opts,noconnection], LL);
remote_spawn_error({'EXIT', {Reason, _}}, _) ->
    {fault, Reason};
remote_spawn_error({'EXIT', Reason}, _) ->
    {fault, Reason};
remote_spawn_error(Other, _) ->
    {fault, Other}.
    
is_well_formed_list([]) ->
    true;
is_well_formed_list([_|Rest]) ->
    is_well_formed_list(Rest);
is_well_formed_list(_) ->
    false.

crasher(Node,Mod,Fun,Args,[],Reason) ->
    error_logger:error_msg("** Can not start ~w:~w,~w on ~w **~n",
			  [ Mod,Fun,Args,Node]),
    exit(Reason);
crasher(Node,Mod,Fun,Args,Opts,Reason) ->
    error_logger:error_msg("** Can not start ~w:~w,~w (~w) on ~w **~n",
			  [ Mod,Fun,Args,Opts,Node]),
    exit(Reason).


yield() ->
    erlang:yield().

nodes() -> erlang:nodes(visible).

disconnect_node(Node) -> net_kernel:disconnect(Node).

fun_info(Fun) when is_function(Fun) ->
    [erlang:fun_info(Fun, Key) ||
	Key <- [pid,module,new_index,new_uniq,index,uniq,env,name,arity]].

send_nosuspend(Pid, Msg) ->
    send_nosuspend(Pid, Msg, []).

send_nosuspend(Pid, Msg, Opts) ->
    case erlang:send(Pid, Msg, [nosuspend|Opts]) of
	ok -> true;
	_  -> false
    end.

localtime_to_universaltime(Localtime) ->
    erlang:localtime_to_universaltime(Localtime, undefined).

%%
%% If the emulator wants to perform a distributed command and
%% a connection is not established to the actual node the following 
%% functions is called in order to set up the connection and then 
%% reactivate the command.
%%

dlink(Pid) ->
    case net_kernel:connect(node(Pid)) of
	true -> link(Pid);
	false -> erlang:dist_exit(self(), noconnection, Pid), true
    end.

%% Can this ever happen?
dunlink(Pid) ->
    case net_kernel:connect(node(Pid)) of
	true -> unlink(Pid);
	false -> true  %% dist_unlink ??
    end.

dmonitor_node(Node, Flag) ->
    case net_kernel:connect(Node) of
	true -> monitor_node(Node, Flag);
	false -> self() ! {nodedown, Node}, true
    end.

dgroup_leader(Leader, Pid) ->
    case net_kernel:connect(node(Pid)) of
	true -> group_leader(Leader, Pid);
	false -> true  %% bad arg ?
    end.

dexit(Pid, Reason) -> 
    case net_kernel:connect(node(Pid)) of
	true -> exit(Pid, Reason);
	false -> true
    end.

dsend(Pid, Msg) when pid(Pid) ->
    case net_kernel:connect(node(Pid)) of
	true -> erlang:send(Pid, Msg);
	false -> Msg
    end;
dsend(Port, Msg) when port(Port) ->
    case net_kernel:connect(node(Port)) of
	true -> erlang:send(Port, Msg);
	false -> Msg
    end;
dsend({Name, Node}, Msg) ->
    case net_kernel:connect(Node) of
	true -> erlang:send({Name,Node}, Msg);
	false -> Msg;
	ignored -> Msg				% Not distributed.
    end.

dsend(Pid, Msg, Opts) when pid(Pid) ->
    case net_kernel:connect(node(Pid)) of
	true -> erlang:send(Pid, Msg, Opts);
	false -> ok
    end;
dsend(Port, Msg, Opts) when port(Port) ->
    case net_kernel:connect(node(Port)) of
	true -> erlang:send(Port, Msg, Opts);
	false -> ok
    end;
dsend({Name, Node}, Msg, Opts) ->
    case net_kernel:connect(Node) of
	true -> erlang:send({Name,Node}, Msg, Opts);
	false -> ok;
	ignored -> ok				% Not distributed.
    end.

dmonitor_p(process, ProcSpec) ->
    {_Proc, Node} =
	case ProcSpec of
	    _ when pid(ProcSpec) ->
		{ProcSpec, node(ProcSpec)};
	    %% N when atom(N) -> will not happen since that
	    %% is a local name and will not require a 
	    %% net_kernel:connect().
	    {S, N} when atom(S), atom(N), N /= node() ->
		ProcSpec
	end,
    case net_kernel:connect(Node) of
	true ->
	    erlang:monitor(process, ProcSpec);
	false ->
	    Ref = make_ref(),
	    DownProcSpec = case ProcSpec of
			       RegName when atom(RegName) ->
				   {RegName, node()};
			       _ -> % {atom(), atom()} | pid()
				   ProcSpec
			   end,
	    self() ! {'DOWN', Ref, process, DownProcSpec, noconnection},
	    Ref
    end.

%%
%% The business with different in and out cookies represented
%% everywhere is discarded.
%% A node has a cookie, connections/messages to that node use that cookie.
%% Messages to us use our cookie. IF we change our cookie, other nodes 
%% have to reflect that, which we cannot forsee.
%%
set_cookie(Node, C) when Node =/= nonode@nohost, atom(Node) ->
    Res = case C of
	      _ when atom(C) ->
		  auth:set_cookie(Node, C);
	      {CI,CO} when atom(CI),atom(CO) ->
		  auth:set_cookie(Node, {CI, CO});
	      _ ->
		  error
	  end,
    case Res of
	error -> exit(badarg);
	Other -> Other
    end.
	    
get_cookie() ->
    auth:get_cookie().

concat_binary(List) ->
    list_to_binary(List).

info(What) ->
    erlang:system_info(What).

%%
%% memory/[0,1]
%%

memory() ->
    erlang:system_info(memory).

memory(Type) when atom(Type) ->
    case erlang:system_info({memory, [Type]}) of
	[{Type, Bytes}] -> Bytes;
	Error -> erlang:fault(Error, [Type])
    end;
memory(TypeSpec) ->
    case erlang:system_info({memory, TypeSpec}) of
	Result when list(Result) -> Result;
	Error -> erlang:fault(Error, [TypeSpec])
    end.



integer_to_list(I, 10) ->
    erlang:integer_to_list(I);
integer_to_list(I, Base) 
  when integer(I), integer(Base), Base >= 2, Base =< 1+$Z-$A+10 ->
    if I < 0 ->
	    [$-|integer_to_list(-I, Base, [])];
       true ->
	    integer_to_list(I, Base, [])
    end;
integer_to_list(I, Base) ->
    erlang:fault(badarg, [I, Base]).

integer_to_list(I0, Base, R0) ->
    D = I0 rem Base,
    I1 = I0 div Base,
    R1 = if D >= 10 ->
		 [D-10+$A|R0];
	    true ->
		 [D+$0|R0]
	 end,
    if I1 == 0 ->
	    R1;
       true ->
	    integer_to_list(I1, Base, R1)
    end.



list_to_integer(L, 10) ->
    erlang:list_to_integer(L);
list_to_integer(L, Base)
  when list(L), integer(Base), Base >= 2, Base =< 1+$Z-$A+10 ->
    case list_to_integer_sign(L, Base) of 
	I when integer(I) ->
	    I;
	Fault ->
	    erlang:fault(Fault, [L,Base])
    end;
list_to_integer(L, Base) ->
    erlang:fault(badarg, [L,Base]).

list_to_integer_sign([$-|[_|_]=L], Base) ->
    case list_to_integer(L, Base, 0) of
	I when integer(I) ->
	    -I;
	I ->
	    I
    end;
list_to_integer_sign([$+|[_|_]=L], Base) ->
    list_to_integer(L, Base, 0);
list_to_integer_sign([_|_]=L, Base) ->
    list_to_integer(L, Base, 0);
list_to_integer_sign(_, _) ->
    badarg.

list_to_integer([D|L], Base, I) 
  when integer(D), D >= $0, D =< $9, D < Base+$0 ->
    list_to_integer(L, Base, I*Base + D-$0);
list_to_integer([D|L], Base, I) 
  when integer(D), D >= $A, D < Base+$A-10 ->
    list_to_integer(L, Base, I*Base + D-$A+10);
list_to_integer([D|L], Base, I) 
  when integer(D), D >= $a, D < Base+$a-10 ->
    list_to_integer(L, Base, I*Base + D-$a+10);
list_to_integer([], _, I) ->
    I;
list_to_integer(_, _, _) ->
    badarg.
