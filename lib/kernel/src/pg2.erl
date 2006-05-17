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
-module(pg2).

-export([create/1, delete/1, join/2, leave/2]).
-export([get_members/1, get_local_members/1]).
-export([get_closest_pid/1, which_groups/0]).
-export([start/0,start_link/0,init/1,handle_call/3,handle_cast/2,handle_info/2,
	 terminate/2]).

%%%-----------------------------------------------------------------
%%% This module implements distributed process groups, in a different
%%% way than the module pg.  pg has a single point of failure (with
%%% one master process).  In pg, each message is sent to all members
%%% of a group.  In this module, each message is sent one, some or all
%%% members of the group.  Which member to send a message to is
%%% decided by the client function.  On each node, a pg2 server process
%%% runs.  Each process group is globally accessible.
%%%-----------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------
-record(state, {links = []}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start() ->
    ensure_started().

create(Name) ->
    ensure_started(),
    case ets:lookup(pg2_table, {local_members, Name}) of
	[] ->
	    global:trans({{?MODULE, Name}, self()},
			 fun() ->
				 gen_server:multi_call(?MODULE, {create, Name})
			 end);
	_ ->
	    ok
    end,
    ok.

delete(Name) ->
    ensure_started(),
    global:trans({{?MODULE, Name}, self()},
		 fun() ->
			 gen_server:multi_call(?MODULE, {delete, Name})
		 end),
    ok.

join(Name, Pid) when pid(Pid) ->
    ensure_started(),
    case ets:lookup(pg2_table, {members, Name}) of
	[] ->
	    {error, {no_such_group, Name}};
	_ ->
	    global:trans({{?MODULE, Name}, self()},
			 fun() ->
				 gen_server:multi_call(?MODULE,
						       {join, Name, Pid})
			 end),
	    ok
    end.

leave(Name, Pid) when pid(Pid) ->
    ensure_started(),
    case ets:lookup(pg2_table, {members, Name}) of
        [] ->
            {error, {no_such_group, Name}};
        _ ->
            global:trans({{?MODULE, Name}, self()},
                         fun() ->
                                 gen_server:multi_call(?MODULE,
                                                       {leave, Name, Pid})
                         end),
            ok
    end.
    
get_members(Name) ->
    ensure_started(),
    case ets:lookup(pg2_table, {members, Name}) of
	[{_, Members}] -> Members;
	[] -> {error, {no_such_group, Name}}
    end.

get_local_members(Name) ->
    ensure_started(),
    case ets:lookup(pg2_table, {local_members, Name}) of
	[{_, Members}] -> Members;
	[] -> {error, {no_such_group, Name}}
    end.

which_groups() ->
    ensure_started(),
    ets:filter(pg2_table,
	       fun([{{members, Group}, _}]) ->
		       {true, Group};
		  (_) ->
		       false
	       end,
	       []).

%%-----------------------------------------------------------------
%% This is a common dispatch function.  If there's a local
%% member, use it.  Otherwise choose one randomly.
%%-----------------------------------------------------------------
get_closest_pid(Name) ->
    case get_local_members(Name) of
	[Pid] ->
	    Pid;
	[] ->
	    {_,_,X} = erlang:now(),
	    case get_members(Name) of
		[] -> {error, {no_process, Name}};
		Members ->
		    lists:nth((X rem length(Members))+1, Members)
	    end;
	Members when list(Members) ->
	    {_,_,X} = erlang:now(),
	    lists:nth((X rem length(Members))+1, Members);
	Else ->
	    Else
    end.

%%%-----------------------------------------------------------------
%%% Callback functions from gen_server
%%%-----------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    Ns = nodes(),
    net_kernel:monitor_nodes(true),
    lists:foreach(fun(N) ->
			  {?MODULE, N} ! {new_pg2, node()},
			  self() ! {nodeup, N}
		  end, Ns),
    % pg2_table keeps track of all members in a group
    ets:new(pg2_table, [set, protected, named_table]),
    {ok, #state{}}.

handle_call({create, Name}, _From, S) ->
    case ets:lookup(pg2_table, {local_members, Name}) of
	[] ->
	    ets:insert(pg2_table, {{local_members, Name}, []}),
	    ets:insert(pg2_table, {{members, Name}, []});
	_ ->
	    ok
    end,
    {reply, ok, S};

handle_call({join, Name, Pid}, _From, S) ->
    case ets:lookup(pg2_table, {members, Name}) of
	[{_, Members}] ->
	    ets:insert(pg2_table, {{members, Name}, [Pid | Members]}),
	    NewLinks =
		if
		    node(Pid) == node() ->
			link(Pid),
			[{_, LocalMembers}] = 
			    ets:lookup(pg2_table, {local_members, Name}),
			ets:insert(pg2_table,
				   {{local_members, Name},
				    [Pid | LocalMembers]}),
			[Pid | S#state.links];
		    true ->
			S#state.links
		end,
	    {reply, ok, S#state{links = NewLinks}};
	[] ->
	    {reply, no_such_group, S}
    end;

handle_call({leave, Name, Pid}, _From, S) ->
    case ets:lookup(pg2_table, {members, Name}) of
        [{_, Members}] ->
            ets:insert(pg2_table, {{members, Name}, lists:delete(Pid,Members)}),
            NewLinks =
                if
                    node(Pid) == node() ->
                        [{_, LocalMembers}] = 
                            ets:lookup(pg2_table, {local_members, Name}),
                        ets:insert(pg2_table,
                                   {{local_members, Name},
                                    lists:delete(Pid, LocalMembers)}),
                        NLinks = lists:delete(Pid, S#state.links),
                        case lists:member(Pid, NLinks) of
                            true -> ok;
                            false -> unlink(Pid)
                        end,
                        NLinks;
                    true ->
                        S#state.links
                end,
            {reply, ok, S#state{links = NewLinks}};
        [] ->
            {reply, no_such_group, S}
    end;
 
handle_call({delete, Name}, _From, S) ->
    ets:delete(pg2_table, {local_members, Name}),
    ets:delete(pg2_table, {members, Name}),
    {reply, ok, S}.

handle_cast({exchange, Node, List}, S) ->
    store(List, Node),
    {noreply, S};

handle_cast({del_member, Name, Pid}, S) ->
    del_member(members, Name, Pid),
    {noreply, S}.

handle_info({'EXIT', Pid, _}, S) ->
    del_members(ets:match(pg2_table, {{local_members, '$1'}, '$2'}), Pid),
    NewLinks = delete(S#state.links, Pid),
    {noreply, S#state{links = NewLinks}};

handle_info({nodeup, Node}, S) ->
    gen_server:cast({?MODULE, Node}, {exchange, node(), all_members()}),
    {noreply, S};

handle_info({new_pg2, Node}, S) ->
    gen_server:cast({?MODULE, Node}, {exchange, node(), all_members()}),
    {noreply, S};

handle_info({nodedown, Node}, S) ->
    del_node_members(ets:match(pg2_table, {{members, '$1'}, '$2'}), Node),
    {noreply, S};

handle_info(_, S) ->
    {noreply, S}.

terminate(_Reason, S) ->
    ets:delete(pg2_table),
    lists:foreach(fun(Pid) -> unlink(Pid) end, S#state.links).

%%%-----------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------

%% Delete member Pid from all groups
del_members([[Name, Pids] | T], Pid) ->
    lists:foreach(
      fun(Pid2) when Pid == Pid2 ->
	      del_member(members, Name, Pid),
	      del_member(local_members, Name, Pid),
	      gen_server:abcast(nodes(), ?MODULE, {del_member, Name, Pid});
	 (_) -> ok
      end, Pids),
    del_members(T, Pid);
del_members([],_Pid) -> ok.

del_member(KeyTag, Name, Pid) ->
    [{_, Members}] = ets:lookup(pg2_table, {KeyTag, Name}),
    ets:insert(pg2_table, {{KeyTag, Name}, delete(Members, Pid)}).

%% Delete all members on Node in all groups.
del_node_members([[Name, Pids] | T], Node) ->
    NewMembers = 
	lists:filter(fun(Pid) when node(Pid) == Node -> false;
			(_) -> true
		     end, Pids),
    ets:insert(pg2_table, {{members, Name}, NewMembers}),
    del_node_members(T, Node);
del_node_members([],_Node) -> ok.

%% delete _all_ occurences of X in list
delete([X | T], X) -> delete(T, X);
delete([H | T], X) -> [H | delete(T, X)];
delete([], _) -> [].

store([[Name, Members] | T], Node) ->
    case ets:lookup(pg2_table, {members, Name}) of
	[] -> 
	    ets:insert(pg2_table, {{members, Name}, Members}),
	    % We can't have any local members, since the group is new to us!
	    ets:insert(pg2_table, {{local_members, Name}, []});
	[{Key, Members2}] ->
	    NInst = union(Members, Members2),
	    ets:insert(pg2_table, {Key, NInst})
    end,
    store(T, Node);
store([], _Node) ->
    ok.

union(L1, L2) ->
    (L1 -- L2) ++ L2.

all_members() ->
    ets:match(pg2_table, {{members, '$1'}, '$2'}).

ensure_started() ->
    case whereis(?MODULE) of
	undefined ->
	    C = {pg2, {?MODULE, start_link, []}, permanent,
		 1000, worker, [?MODULE]},
	    supervisor:start_child(kernel_safe_sup, C);
	Pg2Pid ->
	    {ok, Pg2Pid}
    end.
