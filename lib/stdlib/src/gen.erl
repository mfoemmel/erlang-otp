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
-module(gen).

%%%-----------------------------------------------------------------
%%% This module implements the really generic stuff of the generic
%%% standard behaviours (e.g. gen_server, gen_fsm).
%%%
%%% The standard behaviour should export init_it/6.
%%%-----------------------------------------------------------------
-export([start/5, start/6, debug_options/1,
	 call/3, call/4, reply/2]).

-export([init_it/6, init_it/7]).

-define(default_timeout, 5000).

%%-----------------------------------------------------------------
%% Starts a generic process.
%% start(GenMod, LinkP, Mod, Args, Options)
%% start(GenMod, LinkP, Name, Mod, Args, Options)
%% start_link(Mod, Args, Options)
%% start_link(Name, Mod, Args, Options) where:
%%    Name = {local, atom()} | {global, atom()}
%%    Mod  = atom(), callback module implementing the 'real' fsm
%%    Args = term(), init arguments (to Mod:init/1)
%%    Options = [{debug, [Flag]}]
%%      Flag = trace | log | {logfile, File} | statistics | debug
%%          (debug == log && statistics)
%% Returns: {ok, Pid} |
%%          {error, {already_started, Pid}} |
%%          {error, Reason}
%%-----------------------------------------------------------------
start(GenMod, LinkP, Name, Mod, Args, Options) ->
    case where(Name) of
	undefined ->
	    do_spawn(GenMod, LinkP, Name, Mod, Args, Options);
	Pid ->
	    {error, {already_started, Pid}}
    end.

start(GenMod, LinkP, Mod, Args, Options) ->
    do_spawn(GenMod, LinkP, Mod, Args, Options).

%%-----------------------------------------------------------------
%% Spawn the process (and link) maybe at another node.
%% If spawn without link, set parent to our selves "self"!!!
%%-----------------------------------------------------------------
do_spawn(GenMod, link, Mod, Args, Options) ->
    Time = timeout(Options),
    proc_lib:start_link(gen, init_it,
			[GenMod, self(), self(), Mod, Args, Options], Time);
do_spawn(GenMod, _, Mod, Args, Options) ->
    Time = timeout(Options),
    proc_lib:start(gen, init_it,
		   [GenMod, self(), self, Mod, Args, Options], Time).
do_spawn(GenMod, link, Name, Mod, Args, Options) ->
    Time = timeout(Options),
    proc_lib:start_link(gen, init_it,
			[GenMod, self(), self(), Name, Mod, Args, Options],
			Time);
do_spawn(GenMod, _, Name, Mod, Args, Options) ->
    Time = timeout(Options),
    proc_lib:start(gen, init_it,
		   [GenMod, self(), self, Name, Mod, Args, Options], Time).

%%-----------------------------------------------------------------
%% Initiate the new process.
%% Register the name using the Rfunc function
%% Calls the Mod:init/Args function.
%% Finally an acknowledge is sent to Parent and the main
%% loop is entered.
%%-----------------------------------------------------------------
init_it(GenMod, Starter, Parent, Mod, Args, Options) ->
    init_it2(GenMod, Starter, Parent, self(), Mod, Args, Options).

init_it(GenMod, Starter, Parent, Name, Mod, Args, Options) ->
    case name_register(Name) of
	true ->
	    init_it2(GenMod, Starter, Parent, name(Name), Mod, Args, Options);
	{false, Pid} ->
	    proc_lib:init_ack(Starter, {error, {already_started, Pid}})
    end.

init_it2(GenMod, Starter, Parent, Name, Mod, Args, Options) ->
    GenMod:init_it(Starter, Parent, Name, Mod, Args, Options).


%%-----------------------------------------------------------------
%% Makes a synchronous call to a generic process.
%% Request is sent to the Pid, and the response must be
%% {Tag, _, Reply}.
%%-----------------------------------------------------------------

%%% New call function which uses the new monitor BIF
%%% call(ServerId, Label, Request)

call(Process, Label, Request) -> 
    call(Process, Label, Request, ?default_timeout).

call(Pid, Label, Request, Timeout) when node(Pid) == node() ->
    Mref = erlang:monitor(process,Pid),
    Pid ! {Label, {self(), Mref}, Request},
    wait_resp_mon(Pid, Mref, Timeout);

call(Pid, Label, Request, Timeout) when pid(Pid) ->
    Node = node(Pid),
    set_monitor_node(Node),
    Mref = make_ref(),
    Pid ! {Label, {self(),Mref}, Request},
    Res = wait_resp(Pid, Node, Mref, Timeout),
    reset_monitor_node(Node),
    Res;
call(Name, Label, Request, Timeout) when atom(Name) ->
    Pid = whereis(Name),
    Mref = case catch erlang:monitor(process,Pid) of
	_Mref when reference(_Mref) ->
             _Mref;
	_ -> exit(noproc)
    end,
    Pid ! {Label, {self(),Mref}, Request},
    wait_resp_mon(Pid, Mref, Timeout);
call({global, Name}, Label, Request, Timeout) ->
    Mref = make_ref(),
    case catch global:send(Name, {Label, {self(),Mref}, Request}) of
	Pid when pid(Pid) ->
	    Node = node(Pid),
	    set_monitor_node(Node),
	    Res = wait_resp(Pid, Node, Mref, Timeout),
	    reset_monitor_node(Node),
	    Res;
	{'EXIT', _} ->
	    %% We know that the process won't answer, but we want the same
	    %% semantics for all cases.  Thus, we wait for the reponse...
	    wait_resp(undefined, Mref, Timeout)
    end;
call({Name, Node}, Label, Request, Timeout) when node() == Node ->
    call(Name, Label, Request, Timeout);
call({Name, Node}, Label, Request, Timeout) ->
    set_monitor_node(Node),
    Mref = make_ref(),
    {Name, Node} ! {Label, {self(),Mref}, Request},
    Res = wait_resp(self(), Node, Mref, Timeout), %% dummy pid == self()
    reset_monitor_node(Node),
    Res.


%%
%% If monitor_node/2 tries to set up a connection to
%% a non-existing node (or the connection fails)
%% we dont want to try to set up the connection twice
%% which is done if we first performs a monitor_node and
%% when the distributed send !!
set_monitor_node(Node) ->
    monitor_node(Node, true),
    receive
	{nodedown, Node} -> exit({nodedown, Node})
    after 0              -> ok
    end.

reset_monitor_node(Node) -> monitor_node(Node, false).

wait_resp_mon(Pid, Mref, Timeout) ->
    receive
	{Mref, Reply} ->
	    erlang:demonitor(Mref),
	    receive 
		{'DOWN', Mref, _, _, _Reason} -> true
	    after 0 -> true
	    end,
	    {ok,Reply};
	{'EXIT', Pid, What} ->
	    receive
		{'DOWN', Mref, _, _, _Reason} -> true
	    after 0 -> true
	    end,
	    exit(What);
	{'DOWN', Mref, _, _, Reason} ->
	    receive
		{'EXIT', Pid, What} -> exit(What)
	    after 0 -> exit(Reason)
	    end
    after Timeout ->
	    erlang:demonitor(Mref),
	    receive
		{'DOWN', Mref, _, _, _Reason} -> true 
	    after 0 -> true
	    end,
	    exit(timeout)
    end.

wait_resp(Pid, Tag, Timeout) ->
    receive
	{Tag, Reply} ->
	    {ok,Reply};
	{'EXIT', Pid, What} ->
	    exit(What)
    after Timeout ->
	    exit(timeout)
    end.
wait_resp(Pid, Node, Tag, Timeout) ->
    receive
	{Tag, Reply} ->
	    {ok,Reply};
	{nodedown, Node} ->
	    exit({nodedown, Node});
	{'EXIT', Pid, What} ->
	    exit(What)
    after Timeout ->
	    exit(timeout)
    end.

%
% Send a reply to the client.
%
reply({To, Tag}, Reply) ->
    catch To ! {Tag, Reply}.

%%%-----------------------------------------------------------------
%%%  Misc. functions.
%%%-----------------------------------------------------------------
where({global, Name})    -> global:whereis_name(Name);
where({local, Name})  -> whereis(Name).

name({global, Name})    -> Name;
name({local, Name})     -> Name.

name_register({local, Name}) ->
    case catch register(Name, self()) of
	true -> true;
	{'EXIT', _} ->
	    {false, where({local, Name})}
    end;
name_register({global, Name}) ->
    case global:register_name(Name, self()) of
	yes -> true;
	no -> {false, where({global, Name})}
    end.

timeout(Options) ->
    case opt(timeout, Options) of
	{ok, Time} ->
	    Time;
	_ ->
	    infinity
    end.

opt(Op, [{Op, Value}|Options]) ->
    {ok, Value};
opt(Op, [_|Options]) ->
    opt(Op, Options);
opt(_, []) ->
    false.

debug_options(Opts) ->
    case opt(debug, Opts) of
	{ok, Options} -> sys:debug_options(Options);
	_ -> []
    end.
