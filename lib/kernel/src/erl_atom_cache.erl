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
-module(erl_atom_cache).

-behaviour(gen_server).

%% External exports
-export([start/0, start_link/0, add_node/2, insert/3,
	 fetch/2, lookup/2, delete_node/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-record(state, {cache = ets:new(erl_atom_cache, [public, {keypos, 2},
						 named_table]),
	        node_owners = []}).

-record(node_owner, {pid, node}).

-record(atom, {node_key, value}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start() ->  gen_server:start({local, erl_atom_cache}, erl_atom_cache, [], []).

start_link() ->  gen_server:start_link({local, erl_atom_cache},
				       erl_atom_cache, [], []).

%%
%% Link the Owner. If the Owner terminates all cached atoms
%% for that node are atomatically deleted.
%% Also start the erl_atom_cache server if not started.
%%
add_node(Node, Owner) ->
    start(),
    gen_server:call(erl_atom_cache, {add_node, Node, Owner}, infinity).

%%
%% A Key can either be an Index (for the incomming cache) or the
%% atom itself (for outgoing cache).
%% For the outgoing cache an already existing atom with the same
%% index must be deleted first.
%%
insert(Atom, Node, Index) when list(Atom) ->  %% Outgoing cache
    ets:match_delete(erl_atom_cache, #atom{node_key = {Node, '_'},
					   value = Index}),
    ets:insert(erl_atom_cache, #atom{node_key = {Node, Atom},
				     value = Index});
insert(Index, Node, Atom) when integer(Index) ->  %% Incomming cache
    ets:insert(erl_atom_cache, #atom{node_key = {Node, Index},
				     value = Atom}).

%%
%% A Key is supposed to exist !
%%
fetch(Key, Node) ->
    [Atom] = ets:lookup(erl_atom_cache, {Node, Key}),
    Atom#atom.value.

lookup(Key, Node) ->
    case ets:lookup(erl_atom_cache, {Node, Key}) of
	[Atom] -> {ok, Atom#atom.value};
	_      -> undefined
    end.

delete_node(Node) ->
    ets:match_delete(erl_atom_cache, #atom{node_key = {Node, '_'},
					   value = '_'}).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% 
%%----------------------------------------------------------------------
init([]) ->
    process_flag(trap_exit, true),
    {ok, #state{}}.

%%----------------------------------------------------------------------
%%
%%----------------------------------------------------------------------
handle_call({add_node, Node, Owner}, From, State) ->
    link(Owner),
    Owns = State#state.node_owners,
    {reply, ok, State#state{node_owners =
			    [#node_owner{pid = Owner,
					 node = Node} | Owns]}}.

%%----------------------------------------------------------------------
%%
%%----------------------------------------------------------------------
handle_cast(_, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% 
%%----------------------------------------------------------------------
handle_info({'EXIT', Pid, _}, State) ->
    Owners = State#state.node_owners,
    NewOwn = delete_nodes(Owners, Pid),
    {noreply, State#state{node_owners = NewOwn}};
handle_info(Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% 
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

delete_nodes([#node_owner{pid = Pid, node = Node}|Owners], Pid) ->
    delete_node(Node),
    delete_nodes(Owners, Pid);
delete_nodes([Own|Owners], Pid) ->
    [Own|delete_nodes(Owners, Pid)];
delete_nodes([], _) ->
    [].
    



