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
-module(disk_log_server).
-behaviour(gen_server).

-export([start_link/0, start/0, open/1, close/1, 
	 get_log_pids/1, accessible_logs/0]).

%% Local export.
-export([dist_open/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, terminate/2]).

-include("disk_log.hrl").

-compile({inline,[{do_get_log_pids,1}]}).

%%%-----------------------------------------------------------------
%%% This module implements the disk_log server.  Its primary purpose
%%% is to keep the ets table 'disk_log_names' updated and to handle
%%% distribution data (pids) using the module pg2.
%%%-----------------------------------------------------------------
%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link() ->  
    gen_server:start_link({local, disk_log_server}, disk_log_server, [], []).

start() -> 
    ensure_started().

open({ok, A}) ->
    ensure_started(),
    case gen_server:call(disk_log_server, {open, A}, infinity) of
	{waiting, Pid, From} ->
	    receive {Pid, From, Reply} ->
		    Reply
	    end;
	Reply ->
	    Reply
    end;
open(Other) ->
    Other.

%% To be used from this module only.
dist_open(A) ->
    ensure_started(),
    gen_server:call(disk_log_server, {dist_open, A}, infinity).

close(Pid) ->
    gen_server:call(disk_log_server, {close, Pid}, infinity).

get_log_pids(LogName) ->
    do_get_log_pids(LogName).

accessible_logs() ->
    ensure_started(),
    do_accessible_logs().

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

-define(group(Log), Log).

init([]) ->
    process_flag(trap_exit, true),
    ets:new(?DISK_LOG_NAME_TABLE, [named_table, set]),
    ets:new(?DISK_LOG_PID_TABLE, [named_table, set]),
    {ok, []}.

handle_call({open, A}, From, State) ->
    Reply = do_open(A, From),
    {reply, Reply, State};
handle_call({dist_open, A}, _From, State) ->
    Reply = do_dist_open(A),
    {reply, {node(), Reply}, State};
handle_call({close, Pid}, _From, State) ->
    Reply = do_close(Pid),
    {reply, Reply, State}.

handle_info({'EXIT', Pid, _Reason}, State) ->
    case get(Pid) of
	undefined ->
	    {noreply, State};
	Name ->
	    ets:delete(?DISK_LOG_NAME_TABLE, Name),
	    ets:delete(?DISK_LOG_PID_TABLE, Pid),
	    erase(Pid),
	    {noreply, State}
    end;
handle_info(_, State) ->
    {noreply, State}.
	    
terminate(_Reason, _) ->
    ok.

%%%-----------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------

ensure_started() ->
    case whereis(disk_log_server) of
	undefined ->
	    LogSup = {disk_log_sup, {disk_log_sup, start_link, []}, permanent,
		      1000, supervisor, [disk_log_sup]},
	    supervisor:start_child(kernel_safe_sup, LogSup),
	    LogServer = {disk_log_server,
			 {disk_log_server, start_link, []},
			 permanent, 2000, worker, [disk_log_server]},
	    supervisor:start_child(kernel_safe_sup, LogServer),
	    ok;
	_ -> ok
    end.

do_open(A, From) ->
    Name = A#arg.name,
    {IsDistributed, MyNode} = init_distributed(A),
    case IsDistributed of
	true ->
	    Local = case MyNode of 
			true ->
			    [{node(), do_dist_open(A)}];
			false ->
			    []
		    end,
	    open_distributed(A, Local, From);
	false ->
	    case get_local_pid(Name) of
		{local, Pid} ->
		    disk_log:internal_open(Pid, A);
		{distributed, _Pid} ->
		    {error, {node_already_open, Name}};
		undefined ->
		    case start_log(Name, A) of
			{ok, Pid, R} ->
			    ets:insert(?DISK_LOG_NAME_TABLE, {Name, Pid}),
			    ets:insert(?DISK_LOG_PID_TABLE, {Pid, Name}),
			    R;
			Error ->
			    Error
		    end
	    end
    end.    

% -> OpenRet
do_dist_open(A) ->
    Name = A#arg.name,
    ok = pg2:create(?group(Name)),
    case get_local_pid(Name) of
	undefined ->
	    case start_log(Name, A) of
		{ok, Pid, R} ->
		    ok = pg2:join(?group(Name), Pid),
		    R;
		Error ->
		    Error
	    end;
	{local, _Pid} ->
	    {error, {node_already_open, Name}};
	{distributed, Pid} ->
	    disk_log:internal_open(Pid, A)
    end.

start_log(Name, A) ->
    case supervisor:start_child(disk_log_sup, [self()]) of 
	{ok, Pid} ->
	    link(Pid),
	    put(Pid, Name),
	    case disk_log:internal_open(Pid, A) of
		Error = {error, _} ->
		    Error;
		R ->
		    {ok, Pid, R}
	    end;
	Error ->
	    Error
    end.
    
do_close(Pid) ->
    case get(Pid) of
	undefined ->
	    ok;
	Name ->
	    case get_local_pid(Name) of
		{local, Pid} ->
		    ets:delete(?DISK_LOG_NAME_TABLE, Name),
		    ets:delete(?DISK_LOG_PID_TABLE, Pid);
		{distributed, _Pid} ->
		    ok = pg2:leave(?group(Name), Pid)
	    end,
	    erase(Pid),
	    ok
    end.

do_accessible_logs() ->
    Local0 = lists:map(fun hd/1, ets:match(?DISK_LOG_NAME_TABLE, {'$1','_'})),
    Local = lists:sort(Local0),
    AllDist0 = lists:foldl(fun non_empty_group/2, [], pg2:which_groups()),
    AllDist = lists:sort(AllDist0),
    {Local, ordsets:subtract(AllDist, Local)}.

non_empty_group(?group(G), Gs) ->
    case dist_pids(G) of
	[] -> Gs;
	_ -> [G | Gs]
    end;
non_empty_group(_, Gs) ->
    Gs.

get_local_pid(LogName) ->
    case ets:lookup(?DISK_LOG_NAME_TABLE, LogName) of
	[{_, Pid}] ->
	    {local, Pid};
	[] -> 
	    own_pid(dist_pids(LogName), node())
    end.

own_pid([Pid | _], Node) when node(Pid) == Node ->
    {distributed, Pid};
own_pid([_ | T], Node) ->
    own_pid(T, Node);
own_pid([], _) ->
    undefined.

%% Inlined.
do_get_log_pids(LogName) ->
    case catch ets:lookup(?DISK_LOG_NAME_TABLE, LogName) of
	[{_, Pid}] ->
	    {local, Pid};
	_EmptyOrError -> 
	    case dist_pids(LogName) of
		[] -> undefined;
		Pids  -> {distributed, Pids}
	    end
    end.

dist_pids(LogName) ->
    case catch pg2:get_members(?group(LogName)) of
	Pids when list(Pids) -> Pids;
	_Error -> []
    end.

init_distributed(#arg{distributed = {true, Nodes}}) ->
    {true, lists:member(node(), Nodes)};
init_distributed(_) ->
    {false, false}.

open_distributed(A, Res, From) ->
    {true, N1} = A#arg.distributed,
    Nodes = N1 -- [node()],
    Fun = fun() -> open_distr_rpc(Nodes, A, Res, From) end,
    Pid = spawn(Fun),
    {waiting, Pid, From}.

%% Spawning a process is a means to avoid deadlock when
%% disk_log_servers mutually open disk_logs.
open_distr_rpc(Nodes, A, Res, {FromPid,_Tag} = From) ->
    {Replies, BadNodes} =
	rpc:multicall(Nodes, disk_log_server, dist_open, [A]),
    AllReplies = Res ++ Replies,
    {Ok, Bad} = cr(AllReplies, [], []),
    Old = find_old_nodes(Nodes, AllReplies, BadNodes),
    NotOk = lists:map(fun(BadNode) -> {BadNode, {error, nodedown}} end, 
		      BadNodes ++ Old),
    Reply = {Ok, Bad ++ NotOk},
    %% Send the reply to the waiting client:
    FromPid ! {self(), From, Reply},
    exit(normal).

cr([{badrpc, {'EXIT', _}} | T], Nodes, Bad) ->
    %% This clause can be removed in next release.
    cr(T, Nodes, Bad);
cr([R={_Node, {error, _}} | T], Nodes, Bad) ->  
    cr(T, Nodes, [R | Bad]);
cr([Reply | T], Nodes, Bad) ->  
    cr(T, [Reply | Nodes], Bad);
cr([], Nodes, Bad) -> 
    {Nodes, Bad}.

%% If a "new" node (one that calls dist_open/1) tries to open a log
%% on an old node (one that does not have dist_open/1), then the old
%% node is considered 'down'. In next release, this test will not be
%% needed since all nodes can be assumed to be "new" by then.
%% One more thing: if an old node tries to open a log on a new node,
%% the new node is also considered 'down'.
find_old_nodes(Nodes, Replies, BadNodes) ->
    R = [X || {X, _} <- Replies],
    ordsets:subtract(lists:sort(Nodes), lists:sort(R ++ BadNodes)).
