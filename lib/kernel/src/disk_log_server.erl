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

-export([start_link/0, start/0, open/1, get_log_pids/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, terminate/2]).

-include("disk_log.hrl").

%%%-----------------------------------------------------------------
%%% This module implements the disk_log server.  Its primary purpose
%%% is to keep the ets table 'disk_log_names' updated.
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
    gen_server:call(disk_log_server, {open, A, true}, infinity);
open(Other) ->
    Other.

get_log_pids(LogName) ->
    case catch do_get_log_pids(LogName) of
	{'EXIT', _} ->
	    [];
	Reply ->
	    Reply
    end.

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

init([]) ->
    process_flag(trap_exit, true),
    ets:new(disk_log_names, [public, named_table, set]),
    {ok, []}.

handle_call({open, A, FirstCall}, _From, State) ->
    Name = A#arg.name,
    {IsDistributed, MyNode} = init_distributed(FirstCall, A),
    CreateLog = (not IsDistributed) or MyNode,
    Reply =
	case get_local_pid(Name) of
	    undefined when CreateLog == true -> % I want booleans in guards...
		case supervisor:start_child(disk_log_sup, []) of 
		    {ok, Pid} ->
			link(Pid),
			put(Pid, Name),
			R = disk_log:internal_open(Pid, A),
			if
			    IsDistributed == true ->
				join_distributed(Name, Pid),
				open_distributed(FirstCall,A,[{node(),R}]);
			    true ->
				ets:insert(disk_log_names, {Name, Pid}),
				R
			end;
		    Error ->
			Error
		end;
	    undefined ->
		% IsDistributed == true
		open_distributed(FirstCall, A, []);
	    LogPid ->
		disk_log:internal_open(LogPid, A)
	end,
    {reply, Reply, State}.

handle_info({'EXIT', Pid, _Reason}, State) ->
    case get(Pid) of
	undefined ->
	    {noreply, State};
	Name ->
	    ets:delete(disk_log_names, Name),
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
	    LogServer = {disk_log_server,
			 {disk_log_server, start_link, []},
			 permanent, 2000, worker, [disk_log_server]},
	    LogSup = {disk_log_sup, {disk_log_sup, start_link, []}, permanent,
		      1000, supervisor, [disk_log_sup]},
	    supervisor:start_child(kernel_safe_sup, LogServer),
	    supervisor:start_child(kernel_safe_sup, LogSup);
	_ -> ok
    end.

get_local_pid(Name) ->
    case do_get_log_pids(Name) of
	undefined -> undefined;
	Pids -> own_pid(Pids, node())
    end.

do_get_log_pids(LogName) ->
    case ets:lookup(disk_log_names, LogName) of
	[{_, Pid}] ->
	    [Pid];
	[] -> 
	    case catch pg2:get_members(LogName) of
		[] -> undefined;
		Pids when list(Pids) -> Pids;
		_Error -> undefined
	    end
    end.

own_pid([Pid | _], Node) when node(Pid) == Node ->
    Pid;
own_pid([_ | T], Node) ->
    own_pid(T, Node);
own_pid([], _) ->
    undefined.

init_distributed(true, #arg{distributed = {true, Nodes}, name = Name}) ->
    pg2:create(Name),
    {true, lists:member(node(), Nodes)};
init_distributed(false, #arg{distributed = {true, Nodes}}) ->
    {true, lists:member(node(), Nodes)};
init_distributed(_, _) ->
    {false, false}.

join_distributed(Name, Pid) ->
    pg2:join(Name, Pid).

open_distributed(true, A, Res) ->
    {true, N1} = A#arg.distributed,
    Name = A#arg.name,
    Nodes = N1 -- [node()],
    {Replies, BadNodes} =
	gen_server:multi_call(Nodes, disk_log_server, {open, A, false}),
    check_reply(Res ++ Replies, BadNodes);
open_distributed(false, _, Res) ->
    Res.

check_reply(Res, Bad) ->
    cr(Res, [], 
       lists:map(fun(BadNode) -> {BadNode, {error, nodedown}} end, Bad)).

cr([{Node, {error, R}} | T], Nodes, Bad) -> cr(T, Nodes, [{Node, R} | Bad]);
cr([Ok | T], Nodes, Bad) -> cr(T, [Ok | Nodes], Bad);
cr([], [], Bad) -> Bad;
cr([], Nodes, Bad) -> {Nodes, Bad}.
