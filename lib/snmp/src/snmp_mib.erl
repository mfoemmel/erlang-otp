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
-module(snmp_mib).

%% c(snmp_mib).

%%%-----------------------------------------------------------------
%%% This module implements a MIB server.
%%%-----------------------------------------------------------------

%% External exports
-export([start_link/2, stop/1, lookup/2, next/3, load_mibs/2, unload_mibs/2,
	register_subagent/3, unregister_subagent/2, info/1, info/2]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-include("snmp_types.hrl").

%%-----------------------------------------------------------------
%% Internal Data structures
%%
%%   State
%%       data is the MIB data (defined in snmp_mib_data)
%%-----------------------------------------------------------------
-record(state, {data}).

%%-----------------------------------------------------------------
%% Func: start_link/1
%% Args: Mibs is a list of mibnames.
%% Purpose: starts the mib server synchronized
%% Returns: {ok, Pid} | {error, Reason}
%%-----------------------------------------------------------------
start_link(Mibs, Prio) ->
    gen_server:start_link(snmp_mib, [self(), Mibs, Prio], []).

stop(MibServer) ->
    gen_server:call(MibServer, stop, infinity).

%%--------------------------------------------------
%% The standard MIB 'stdmib' must be present in the
%% current directory.
%%--------------------------------------------------
init([Father, Mibs, Prio]) ->
    process_flag(priority, Prio),
    process_flag(trap_exit, true),
    Data = snmp_mib_data:new(),
    case mib_operations(load_mib, Mibs, Data) of
	{ok, Data2} ->
	    {ok, #state{data = Data2}};
	{'aborted at', Mib, _NewData, Reason} ->
	    {stop, {Mib, Reason}}
    end.

%%----------------------------------------------------------------------
%% Returns: {ok, NewMibData} | {'aborted at', Mib, NewData, Reason}
%% Args: Operation is load_mib | unload_mib.
%%----------------------------------------------------------------------
mib_operations(Operation, [Mib | Mibs], Data) when list(Mib) ->
    case apply(snmp_mib_data, Operation, [Data, Mib]) of
	{error, Reason} ->
	    {'aborted at', Mib, Data, Reason};
	NewData ->
	    mib_operations(Operation, Mibs, NewData)
    end;
mib_operations(Operation, [Mib | Mibs], Data) ->
    {'aborted at', Mib,Data,bad_mibname};
mib_operations(Operation, [], Data) -> {ok, Data}.

%%-----------------------------------------------------------------
%% Func: lookup/2
%% Purpose: Finds the mib entry corresponding to the Oid. If it is a
%%          variable, the Oid must be <Oid for var>.0 and if it is
%%          a table, Oid must be <table>.<entry>.<col>.<any>
%% Returns: {variable, MibEntry} |
%%          {table_column, MibEntry, TableEntryOid} |
%%          {subagent, SubAgentPid} |
%%          false
%%-----------------------------------------------------------------
lookup(MibServer, Oid) ->
    gen_server:call(MibServer, {lookup, Oid}, infinity).

%%-----------------------------------------------------------------
%% Func: next/3
%% Purpose: Finds the lexicographically next oid.
%% Returns: {subagent, SubAgentPid, SANextOid} |
%%          endOfMibView |
%%          genErr |
%%          NextOid
%%   The SANextOid is used by the agent if the SubAgent returns
%%   endOfMib, in a new call to next/2.
%%-----------------------------------------------------------------
next(MibServer, Oid, MibView) ->
    gen_server:call(MibServer, {next, Oid, MibView}, infinity).

%%----------------------------------------------------------------------
%% Purpose: Loads mibs into the mib process.
%% Args: Mibs is a list of Filenames (compiled mibs).
%% Returns: ok | {error, Reason}
%%----------------------------------------------------------------------
load_mibs(MibServer, Mibs) ->
    gen_server:call(MibServer, {load_mibs, Mibs}, infinity).

%%----------------------------------------------------------------------
%% Purpose: Loads mibs into the mib process.
%% Args: Mibs is a list of Filenames (compiled mibs).
%% Returns: ok | {error, Reason}
%%----------------------------------------------------------------------
unload_mibs(MibServer, Mibs) ->
    gen_server:call(MibServer, {unload_mibs, Mibs}, infinity).

%%----------------------------------------------------------------------
%% Registers subagent with pid Pid under subtree Oid.
%%----------------------------------------------------------------------
register_subagent(MibServer, Oid, Pid) ->
    gen_server:call(MibServer, {register_subagent, Oid, Pid}, infinity).

unregister_subagent(MibServer, OidOrPid) ->
    gen_server:call(MibServer, {unregister_subagent, OidOrPid}, infinity).

info(MibServer) ->
    gen_server:call(MibServer, info, infinity).

info(MibServer, Type) ->
    gen_server:call(MibServer, {info, Type}, infinity).

%%-----------------------------------------------------------------
%% Handle messages
%%-----------------------------------------------------------------
handle_call({lookup, Oid}, _From, State) ->
    Data = State#state.data,
    Reply = snmp_mib_data:lookup(Data, Oid),
    {reply, Reply, State};

handle_call({next, Oid, MibView}, _From, State) ->
    Data = State#state.data,
    Reply = snmp_mib_data:next(Data, Oid, MibView),
    {reply, Reply, State};

handle_call({load_mibs, Mibs}, _From, State) ->
    case mib_operations(load_mib, Mibs, State#state.data) of
	{'aborted at', Mib, NewData, Reason} ->
	    R = {error, {'load aborted at', Mib, Reason}};
	{ok, NewData} ->
	    R = ok
    end,
    {reply, R, State#state{data = NewData}};

handle_call({unload_mibs, Mibs}, _From, State) ->
    case mib_operations(unload_mib, Mibs, State#state.data) of
	{'aborted at', Mib, NewData, Reason} ->
	    R = {error, {'unload aborted at', Mib, Reason}};
	{ok, NewData} ->
	    R = ok
    end,
    {reply, R, State#state{data = NewData}};

handle_call({register_subagent, Oid, Pid}, _From, State) ->
    case snmp_mib_data:register_subagent(State#state.data, Oid, Pid) of
	{error, Reason} ->
	    {reply, {error, Reason}, State};
	NewData ->
	    {reply, ok, State#state{data = NewData}}
    end;

handle_call({unregister_subagent, OidOrPid}, _From, State) ->
    case snmp_mib_data:unregister_subagent(State#state.data, OidOrPid) of
	{ok, NewData, DeletedSubagentPid} ->
	    {reply, {ok, DeletedSubagentPid}, State#state{data=NewData}};
	{error, Reason} ->
	    {reply, {error, Reason}, State};
	NewData ->
	    {reply, ok, State#state{data = NewData}}
    end;

handle_call(info, _From, State) ->
    {reply, snmp_mib_data:info(State#state.data), State};

handle_call({info, Type}, _From, State) ->
    {reply, catch snmp_mib_data:info(State#state.data, Type), State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.
    
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    catch snmp_mib_data:unload_all(State#state.data),
    ok.
