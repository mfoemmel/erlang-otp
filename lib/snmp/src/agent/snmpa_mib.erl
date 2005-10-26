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
-module(snmpa_mib).

%% c(snmpa_mib).

%%%-----------------------------------------------------------------
%%% This module implements a MIB server.
%%%-----------------------------------------------------------------

%% External exports
-export([start_link/3, stop/1, 
	 lookup/2, next/3, which_mib/2, which_mibs/1, whereis_mib/2, 
	 load_mibs/2, unload_mibs/2, 
	 register_subagent/3, unregister_subagent/2, info/1, info/2, 
	 verbosity/2, dump/1, dump/2]).

%% Internal exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
	 code_change/3]).

-include("snmp_types.hrl").
-include("snmp_verbosity.hrl").
-include("snmp_debug.hrl").


-define(SERVER, ?MODULE).

-ifdef(snmp_debug).
-define(GS_START_LINK(Prio, Mibs, Opts),
        gen_server:start_link(?MODULE, [Prio, Mibs, Opts], [{debug,[trace]}])).
-else.
-define(GS_START_LINK(Prio, Mibs, Opts),
        gen_server:start_link(?MODULE, [Prio, Mibs, Opts], [])).
-endif.
 

%%-----------------------------------------------------------------
%% Internal Data structures
%%
%%   State
%%       data - is the MIB data (defined in snmpa_mib_data)
%%       meo  - mib entry override
%%       teo  - trap (notification) entry override
%%-----------------------------------------------------------------
-record(state, {data, meo, teo}).



%%-----------------------------------------------------------------
%% Func: start_link/1
%% Args: Mibs is a list of mibnames.
%%       Prio is priority of mib-server
%%       Opts is a list of options
%% Purpose: starts the mib server synchronized
%% Returns: {ok, Pid} | {error, Reason}
%%-----------------------------------------------------------------
start_link(Prio, Mibs, Opts) ->
    ?d("start_link -> entry with"
	"~n   Prio: ~p"
	"~n   Mibs: ~p"
	"~n   Opts: ~p",[Prio, Mibs, Opts]),
    ?GS_START_LINK(Prio, Mibs, Opts).

verbosity(MibServer, Verbosity) -> 
    cast(MibServer, {verbosity,Verbosity}).

stop(MibServer) ->
    call(MibServer, stop).


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
    call(MibServer, {lookup, Oid}).

which_mib(MibServer, Oid) ->
    call(MibServer, {which_mib, Oid}).


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
    call(MibServer, {next, Oid, MibView}).


%%----------------------------------------------------------------------
%% Purpose: Loads mibs into the mib process.
%% Args: Mibs is a list of Filenames (compiled mibs).
%% Returns: ok | {error, Reason}
%%----------------------------------------------------------------------
load_mibs(MibServer, Mibs) ->
    call(MibServer, {load_mibs, Mibs}).


%%----------------------------------------------------------------------
%% Purpose: Loads mibs into the mib process.
%% Args: Mibs is a list of Filenames (compiled mibs).
%% Returns: ok | {error, Reason}
%%----------------------------------------------------------------------
unload_mibs(MibServer, Mibs) ->
    call(MibServer, {unload_mibs, Mibs}).


%%----------------------------------------------------------------------
%% Purpose: Simple management functions
%% Args: Mib is the name of the mib (atom)
%% Returns: ok | {error, Reason}
%%----------------------------------------------------------------------
which_mibs(MibServer) ->
    call(MibServer, which_mibs).

whereis_mib(MibServer, Mib) ->
    call(MibServer, {whereis_mib, Mib}).


%%----------------------------------------------------------------------
%% Registers subagent with pid Pid under subtree Oid.
%%----------------------------------------------------------------------
register_subagent(MibServer, Oid, Pid) ->
    call(MibServer, {register_subagent, Oid, Pid}).

unregister_subagent(MibServer, OidOrPid) ->
    call(MibServer, {unregister_subagent, OidOrPid}).

info(MibServer) ->
    call(MibServer, info).

info(MibServer, Type) ->
    call(MibServer, {info, Type}).

dump(MibServer) ->
    call(MibServer, dump).

dump(MibServer,File) when list(File) ->
    call(MibServer, {dump,File}).


%%--------------------------------------------------
%% The standard MIB 'stdmib' must be present in the
%% current directory.
%%--------------------------------------------------
init([Prio, Mibs, Opts]) ->
    ?d("init -> entry with"
	"~n   Prio: ~p"
	"~n   Mibs: ~p"
	"~n   Opts: ~p",[Prio, Mibs, Opts]),
    case (catch do_init(Prio, Mibs, Opts)) of
	{ok, State} ->
	    {ok, State};
	{error, Reason} ->
	    config_err("failed starting mib-server: ~n~p", [Reason]),
	    {stop, {error, Reason}};
	Error ->
	    config_err("failed starting mib-server: ~n~p", [Error]),
	    {stop, {error, Error}}
    end.

do_init(Prio, Mibs, Opts) ->
    process_flag(priority, Prio),
    process_flag(trap_exit, true),
    put(sname,ms),
    put(verbosity,?vvalidate(get_verbosity(Opts))),
    ?vlog("starting",[]),
    MeOverride = get_me_override(Opts),
    TeOverride = get_te_override(Opts),
    MibStorage = get_mib_storage(Opts),
    Data       = snmpa_mib_data:new(MibStorage),
    ?vtrace("init -> mib data created",[]),
    case (catch mib_operations(load_mib, Mibs, Data, 
			       MeOverride, TeOverride, true)) of
	{ok, Data2} ->
	    ?vdebug("started",[]),
	    snmpa_mib_data:store(Data2),
	    ?vdebug("mib data stored",[]),
	    {ok, #state{data = Data2, teo = TeOverride, meo = MeOverride}};
	{'aborted at', Mib, _NewData, Reason} ->
	    ?vinfo("failed loading mib ~p: ~p",[Mib,Reason]),
	    {error, {Mib, Reason}}
    end.


%%----------------------------------------------------------------------
%% Returns: {ok, NewMibData} | {'aborted at', Mib, NewData, Reason}
%% Args: Operation is load_mib | unload_mib.
%%----------------------------------------------------------------------
mib_operations(Operation, Mibs, Data, MeOverride, TeOverride) ->
    mib_operations(Operation, Mibs, Data, MeOverride, TeOverride, false). 


mib_operations(_Operation, [], Data, _MeOverride, _TeOverride, _Force) ->
    {ok, Data};
mib_operations(Operation, [Mib|Mibs], Data0, MeOverride, TeOverride, Force) ->
    ?vtrace("mib operations ~p on"
	"~n   Mibs: ~p"
	"~n   with "
	"~n   MeOverride: ~p"
	"~n   TeOverride: ~p"
	"~n   Force:      ~p", [Operation,Mibs,MeOverride,TeOverride,Force]),
    Data = mib_operation(Operation, Mib, Data0, MeOverride, TeOverride, Force),
    mib_operations(Operation, Mibs, Data, MeOverride, TeOverride, Force).

mib_operation(Operation, Mib, Data0, MeOverride, TeOverride, Force) 
  when list(Mib) ->
    ?vtrace("mib operation on mib ~p", [Mib]),
    case apply(snmpa_mib_data, Operation, [Data0,Mib,MeOverride,TeOverride]) of
	{error, 'already loaded'} when Operation == load_mib, 
				       Force == true ->
	    ?vlog("ignore mib ~p -> already loaded", [Mib]),
	    Data0;
	{error, 'not loaded'} when Operation == unload_mib, 
				   Force == true ->
	    ?vlog("ignore mib ~p -> not loaded", [Mib]),
	    Data0;
	{error, Reason} ->
	    ?vlog("mib_operation -> failed ~p of mib ~p for ~p", 
		[Operation, Mib, Reason]),
	    throw({'aborted at', Mib, Data0, Reason});
	{ok, Data} ->
	    Data
    end;
mib_operation(_Op, Mib, Data, _MeOverride, _TeOverride, _Force) ->
    throw({'aborted at', Mib, Data, bad_mibname}).


%%-----------------------------------------------------------------
%% Handle messages
%%-----------------------------------------------------------------
handle_call({lookup, Oid}, _From, #state{data = Data} = State) ->
    ?vlog("lookup ~p",[Oid]),    
    Reply = snmpa_mib_data:lookup(Data, Oid),
    {reply, Reply, State};

handle_call({which_mib, Oid}, _From, #state{data = Data} = State) ->
    ?vlog("which_mib ~p",[Oid]),    
    Reply = snmpa_mib_data:which_mib(Data, Oid),
    ?vdebug("which_mib: ~p",[Reply]),    
    {reply, Reply, State};

handle_call({next, Oid, MibView}, _From, #state{data = Data} = State) ->
    ?vlog("next ~p [~p]",[Oid,MibView]),    
    Reply = snmpa_mib_data:next(Data, Oid, MibView),
    ?vdebug("next: ~p",[Reply]),    
    {reply, Reply, State};

handle_call({load_mibs, Mibs}, _From, 
	    #state{data = Data, teo = TeOverride, meo = MeOverride} = State) ->
    ?vlog("load mibs ~p",[Mibs]),    
    {NData,Reply} = 
	case (catch mib_operations(load_mib, Mibs, Data,
				   MeOverride, TeOverride)) of
	    {'aborted at', Mib, NewData, Reason} ->
		?vlog("aborted at ~p for reason ~p",[Mib,Reason]),    
		{NewData,{error, {'load aborted at', Mib, Reason}}};
	    {ok, NewData} ->
		{NewData,ok}
	end,
    snmpa_mib_data:store(NData),
    {reply, Reply, State#state{data = NData}};

handle_call({unload_mibs, Mibs}, _From, 
	    #state{data = Data, teo = TeOverride, meo = MeOverride} = State) ->
    ?vlog("unload mibs ~p",[Mibs]),    
    {NData,Reply} = 
	case (catch mib_operations(unload_mib, Mibs, Data,
				   MeOverride, TeOverride)) of
	    {'aborted at', Mib, NewData, Reason} ->
		?vlog("aborted at ~p for reason ~p",[Mib,Reason]),    
		{NewData,{error, {'unload aborted at', Mib, Reason}}};
	    {ok, NewData} ->
		{NewData,ok}
	end,
    snmpa_mib_data:store(NData),
    {reply, Reply, State#state{data = NData}};

handle_call(which_mibs, _From, #state{data = Data} = State) ->
    ?vlog("which mibs",[]),    
    Reply = snmpa_mib_data:which_mibs(Data),
    {reply, Reply, State};

handle_call({whereis_mib, Mib}, _From, #state{data = Data} = State) ->
    ?vlog("whereis mib: ~p",[Mib]),    
    Reply = snmpa_mib_data:whereis_mib(Data, Mib),
    {reply, Reply, State};

handle_call({register_subagent, Oid, Pid}, _From, State) ->
    ?vlog("register subagent ~p, ~p",[Oid,Pid]),
    case snmpa_mib_data:register_subagent(State#state.data, Oid, Pid) of
	{error, Reason} ->
	    ?vlog("registration failed: ~p",[Reason]),    
	    {reply, {error, Reason}, State};
	NewData ->
	    {reply, ok, State#state{data = NewData}}
    end;

handle_call({unregister_subagent, OidOrPid}, _From, State) ->
    ?vlog("unregister subagent ~p",[OidOrPid]),    
    case snmpa_mib_data:unregister_subagent(State#state.data, OidOrPid) of
	{ok, NewData, DeletedSubagentPid} ->
	    {reply, {ok, DeletedSubagentPid}, State#state{data=NewData}};
	{error, Reason} ->
	    ?vlog("unregistration failed: ~p",[Reason]),    
	    {reply, {error, Reason}, State};
	NewData ->
	    {reply, ok, State#state{data = NewData}}
    end;

handle_call(info, _From, #state{data = Data} = State) ->
    ?vlog("info",[]),    
    {reply, catch snmpa_mib_data:info(Data), State};

handle_call({info, Type}, _From, #state{data = Data} = State) ->
    ?vlog("info ~p",[Type]),    
    Reply = case (catch snmpa_mib_data:info(Data, Type)) of
		Info when list(Info) ->
		    Info;
		E ->
		    [{error, E}]
	    end,
    {reply, Reply, State};

handle_call(dump, _From, State) ->
    ?vlog("dump",[]),    
    Reply = snmpa_mib_data:dump(State#state.data),
    {reply, Reply, State};
    
handle_call({dump,File}, _From, #state{data = Data} = State) ->
    ?vlog("dump on ~s",[File]),    
    Reply = snmpa_mib_data:dump(Data,File),
    {reply, Reply, State};
    
handle_call(stop, _From, State) ->
    ?vlog("stop",[]),    
    {stop, normal, ok, State};

handle_call(Req, _From, State) ->
    info_msg("received unknown request: ~n~p", [Req]),
    Reply = {error, {unknown, Req}}, 
    {reply, Reply, State}.
    
handle_cast({verbosity,Verbosity}, State) ->
    ?vlog("verbosity: ~p -> ~p",[get(verbosity),Verbosity]),    
    put(verbosity,snmp_verbosity:validate(Verbosity)),
    {noreply, State};
    
handle_cast(Msg, State) ->
    info_msg("received unknown message: ~n~p", [Msg]),
    {noreply, State}.
    
handle_info(Info, State) ->
    info_msg("received unknown info: ~n~p", [Info]),
    {noreply, State}.

terminate(_Reason, #state{data = Data}) ->
    catch snmpa_mib_data:close(Data),
    ok.



%%----------------------------------------------------------
%% Code change
%%----------------------------------------------------------

%% downgrade
code_change({down, _Vsn}, #state{data = Data} = State, _Extra) ->
    ?d("code_change(down) -> entry with~n"
	"  Vsn: ~p", [_Vsn]),
    NData = snmpa_mib_data:code_change(down, Data),
    {ok, State#state{data = NData}};

%% upgrade
code_change(_Vsn, #state{data = Data} = State, _Extra) ->
    ?d("code_change(up) -> entry with~n"
	"  Vsn: ~p",
	[_Vsn]),
    NData = snmpa_mib_data:code_change(up, Data),
    ?d("code_change(up) -> New Mib data created~n",[]),
    {ok, State#state{data = NData}}.



%%-----------------------------------------------------------------
%% Option access functions
%%-----------------------------------------------------------------

get_verbosity(O) ->
    snmp_misc:get_option(verbosity,O,?default_verbosity).

get_me_override(O) ->
    snmp_misc:get_option(mibentry_override,O,false).

get_te_override(O) ->
    snmp_misc:get_option(trapentry_override,O,false).

get_mib_storage(O) ->
    snmp_misc:get_option(mib_storage,O,ets).


%% ----------------------------------------------------------------

cast(MibServer, Msg) ->
    gen_server:cast(MibServer, Msg).

call(MibServer, Req) ->
    call(MibServer, Req, infinity).

call(MibServer, Req, To) ->
    gen_server:call(MibServer, Req, To).


%% ----------------------------------------------------------------

info_msg(F, A) ->
    error_logger:info_msg("~w: " ++ F ++ "~n", [?MODULE|A]).

config_err(F, A) ->
    snmpa_error:config_err(F, A).
 
