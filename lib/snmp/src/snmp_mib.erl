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
-export([start_link/2, start_link/3, stop/1, 
	 lookup/2, next/3, load_mibs/2, unload_mibs/2,
	 register_subagent/3, unregister_subagent/2, info/1, info/2, 
	 verbosity/2, dump/1, dump/2]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-include("snmp_types.hrl").
-include("snmp_verbosity.hrl").
-include("snmp_debug.hrl").

-ifndef(default_verbosity).
-define(default_verbosity,silence).
-endif.

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
%%       Prio is priority of mib-server
%%       Opts is a list of options
%% Purpose: starts the mib server synchronized
%% Returns: {ok, Pid} | {error, Reason}
%%-----------------------------------------------------------------
start_link(Mibs, Prio) ->
    start_link(Mibs,Prio,[]).

start_link(Mibs, Prio, Opts) ->
    gen_server:start_link(snmp_mib, [self(), Mibs, Prio, Opts], []).

verbosity(Pid,Verbosity) -> 
    gen_server:cast(Pid,{verbosity,Verbosity}).

stop(MibServer) ->
    gen_server:call(MibServer, stop, infinity).

%%--------------------------------------------------
%% The standard MIB 'stdmib' must be present in the
%% current directory.
%%--------------------------------------------------
init([Father, Mibs, Prio, Opts]) ->
    process_flag(priority, Prio),
    process_flag(trap_exit, true),
    put(sname,ms),
    put(verbosity,?vvalidate(get_verbosity(Opts))),
    MeOverride = get_me_override(Opts),
    put(me_override,MeOverride),
    TeOverride = get_te_override(Opts),
    put(te_override,TeOverride),
    ?vlog("starting",[]),
    Data = snmp_mib_data:new(),
    case mib_operations(load_mib, Mibs, Data, MeOverride, TeOverride) of
	{ok, Data2} ->
	    ?vdebug("started",[]),
	    {ok, #state{data = Data2}};
	{'aborted at', Mib, _NewData, Reason} ->
	    ?vinfo("aborted at ~p for reason ~p",[Mib,Reason]),
	    {stop, {Mib, Reason}}
    end.

%%----------------------------------------------------------------------
%% Returns: {ok, NewMibData} | {'aborted at', Mib, NewData, Reason}
%% Args: Operation is load_mib | unload_mib.
%%----------------------------------------------------------------------
mib_operations(Operation, [Mib | Mibs], Data, MeOverride, TeOverride) when list(Mib) ->
    case apply(snmp_mib_data, Operation, [Data,Mib,MeOverride,TeOverride]) of
	{error, Reason} ->
	    {'aborted at', Mib, Data, Reason};
	NewData ->
	    mib_operations(Operation, Mibs, NewData, MeOverride, TeOverride)
    end;
mib_operations(Operation, [Mib | Mibs], Data, MeOverride, TeOverride) ->
    {'aborted at', Mib,Data,bad_mibname};
mib_operations(Operation, [], Data, MeOverride, TeOverride) -> {ok, Data}.

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

dump(MibServer) ->
    gen_server:call(MibServer, dump, infinity).

dump(MibServer,File) when list(File) ->
    gen_server:call(MibServer, {dump,File}, infinity).

%%-----------------------------------------------------------------
%% Handle messages
%%-----------------------------------------------------------------
handle_call({lookup, Oid}, _From, State) ->
    ?vlog("lookup ~p",[Oid]),    
    Data = State#state.data,
    Reply = snmp_mib_data:lookup(Data, Oid),
    {reply, Reply, State};

handle_call({next, Oid, MibView}, _From, State) ->
    ?vlog("next ~p",[Oid]),    
    Data = State#state.data,
    Reply = snmp_mib_data:next(Data, Oid, MibView),
    {reply, Reply, State};

handle_call({load_mibs, Mibs}, _From, State) ->
    ?vlog("load mibs ~p",[Mibs]),    
    MeOverride = get(me_override), 
    TeOverride = get(te_override),
    case mib_operations(load_mib,Mibs,State#state.data,MeOverride,TeOverride) of
	{'aborted at', Mib, NewData, Reason} ->
	    ?vlog("aborted at ~p for reason ~p",[Mib,Reason]),    
	    R = {error, {'load aborted at', Mib, Reason}};
	{ok, NewData} ->
	    R = ok
    end,
    {reply, R, State#state{data = NewData}};

handle_call({unload_mibs, Mibs}, _From, State) ->
    ?vlog("unload mibs ~p",[Mibs]),    
    MeOverride = get(me_override), 
    TeOverride = get(te_override),
    case mib_operations(unload_mib,Mibs,State#state.data,MeOverride,TeOverride) of
	{'aborted at', Mib, NewData, Reason} ->
	    ?vlog("aborted at ~p for reason ~p",[Mib,Reason]),    
	    R = {error, {'unload aborted at', Mib, Reason}};
	{ok, NewData} ->
	    R = ok
    end,
    {reply, R, State#state{data = NewData}};

handle_call({register_subagent, Oid, Pid}, _From, State) ->
    ?vlog("register subagent ~p, ~p",[Oid,Pid]),
    case snmp_mib_data:register_subagent(State#state.data, Oid, Pid) of
	{error, Reason} ->
	    ?vlog("registration failed: ~p",[Reason]),    
	    {reply, {error, Reason}, State};
	NewData ->
	    {reply, ok, State#state{data = NewData}}
    end;

handle_call({unregister_subagent, OidOrPid}, _From, State) ->
    ?vlog("unregister subagent ~p",[OidOrPid]),    
    case snmp_mib_data:unregister_subagent(State#state.data, OidOrPid) of
	{ok, NewData, DeletedSubagentPid} ->
	    {reply, {ok, DeletedSubagentPid}, State#state{data=NewData}};
	{error, Reason} ->
	    ?vlog("unregistration failed: ~p",[Reason]),    
	    {reply, {error, Reason}, State};
	NewData ->
	    {reply, ok, State#state{data = NewData}}
    end;

handle_call(info, _From, State) ->
    ?vlog("info",[]),    
    {reply, snmp_mib_data:info(State#state.data), State};

handle_call({info, Type}, _From, State) ->
    ?vlog("info ~p",[Type]),    
    {reply, catch snmp_mib_data:info(State#state.data, Type), State};

handle_call(dump, _From, State) ->
    ?vlog("dump",[]),    
    Reply = snmp_mib_data:dump(State#state.data),
    {reply, Reply, State};
    
handle_call({dump,File}, _From, State) ->
    ?vlog("dump on ~s",[File]),    
    Reply = snmp_mib_data:dump(State#state.data,File),
    {reply, Reply, State};
    
handle_call(stop, _From, State) ->
    ?vlog("stop",[]),    
    {stop, normal, ok, State}.

handle_cast({verbosity,Verbosity}, State) ->
    ?vlog("verbosity: ~p -> ~p",[get(verbosity),Verbosity]),    
    put(verbosity,snmp_verbosity:validate(Verbosity)),
    {noreply, State};
    
handle_cast(_, State) ->
    {noreply, State}.
    
handle_info(_, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    catch snmp_mib_data:unload_all(State#state.data),
    ok.



%%----------------------------------------------------------
%% Code change
%%----------------------------------------------------------

%% downgrade
code_change({down, Vsn}, State, downgrade_to_pre_3_2_0) ->
    ?debug("code_change(down) -> entry with~n"
	   "  Vsn:   ~p~n"
	   "  State: ~p~n"
	   "  Extra: ~p",
	   [Vsn,State,downgrade_to_pre_3_2_0]),
    NData = snmp_mib_data:code_change({down,pre_3_2_0},State#state.data),
    {ok, State#state{data = NData}};
code_change({down, Vsn}, State, Extra) ->
    ?debug("code_change(down) -> entry with~n"
	   "  Vsn:   ~p~n"
	   "  State: ~p~n"
	   "  Extra: ~p",
	   [Vsn,State,Extra]),
    {ok, State};

%% upgrade
code_change(Vsn, State, upgrade_from_pre_3_2_0) ->
    ?debug("code_change(up) -> entry with~n"
	   "  Vsn:   ~p~n"
	   "  State: ~p~n"
	   "  Extra: ~p",
	   [Vsn,State,upgrade_from_pre_3_2_0]),
    NData = snmp_mib_data:code_change({up,pre_3_2_0},State#state.data),
    {ok, State#state{data = NData}};
code_change(Vsn, State, Extra) ->
    ?debug("code_change(up) -> entry with~n"
	   "  Vsn:   ~p~n"
	   "  State: ~p~n"
	   "  Extra: ~p",
	   [Vsn,State,Extra]),
    {ok, State}.



%%-----------------------------------------------------------------
%% Option access functions
%%-----------------------------------------------------------------

get_verbosity(O) ->
    snmp_misc:get_option(mibserver_verbosity,O,?default_verbosity).

get_me_override(O) ->
    snmp_misc:get_option(mibentry_override,O,false).

get_te_override(O) ->
    snmp_misc:get_option(trapentry_override,O,false).


