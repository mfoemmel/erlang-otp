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
%%%----------------------------------------------------------------------
%%% Purpose : Fixtable server for (d)ets:safe_fixtable only for internal
%%%           use by these modules.
%%% Note    : This module contains both a supervisor bridge and
%%%           a "non gen-server" server, the fixtable_server.
%%%           This is because I want proper behaviour from the
%%%           kernel supervisors point of view as well as the performance
%%%           of a "home made" server. This may look strange in appmon,
%%%           but should be the correct way to do it.
%%%           The server responds to system messages.
%%%----------------------------------------------------------------------

-module(fixtable_server).
-behaviour(supervisor_bridge).

%%%-compile(export_all).
%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
-export([safe_fixtable/3, safe_fixtable/4, 
	 table_closed/2, table_closed/3, 
	 info/2]).
%%%----------------------------------------------------------------------
%%% Exports for internal supervisor_bridbe behaviour
%%%----------------------------------------------------------------------
-export([start_link/0, init/1, terminate/2]).
%%%----------------------------------------------------------------------
%%% Exports for intyernal use
%%%----------------------------------------------------------------------
-export([server_init/1]).
%%%----------------------------------------------------------------------
%%% Exports for sys:handle_system_msg/6
%%%----------------------------------------------------------------------
-export([system_continue/3, system_terminate/4, system_code_change/4]).

%%%----------------------------------------------------------------------
%%% Macros
%%%----------------------------------------------------------------------
-define(PROCNAME,fixtable_server). % The registered name of the server process
-define(PROCNAME_SUP,fixtable_server_sup). % The registered name of the 
                                           % supervisor bridge process
-define(CALL_TIMEOUT, 10000). % Timeout for calls to server
%% Table options that are appended to the creation options for all internal
%% state tables (used to make all public during debugging etc)
-define(COMMON_TABLE_OPTIONS, [protected]).



%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% safe_fixtable(Module, Tab, Bool) -> true
%%% Module = dets | ets
%%% Tab = atom() | integer() | pid() (Table identifier for dets or ets table)
%%% Bool = true | false
%%% Possibly calls Module:fixtable(Tab,Bool) if we are the first to 
%%% try to fix the table or the last to try to release.
%%% NOTE! The caller must ensure that the process has write access
%%% to the table if cleanup on exit shall work.
%%%----------------------------------------------------------------------
safe_fixtable(Module, Tab, Bool, RPid) ->
    Owner = case Module:info(Tab,owner) of
	      Pid when pid(Pid) ->
		    Pid;
		_ ->
		    exit({badarg, {Module, safe_fixtable, [Tab, Bool]}})
	    end,
    case call({{Module, Tab}, Bool, Owner, RPid}) of
	{error, undefined} ->
	    undefined;
	{error, Reason} ->
	    exit(Reason);
	{true, fixit} ->
	    Module:fixtable(Tab, true),
	    ack(),
	    true;
	{true, releaseit} ->
	    Module:fixtable(Tab, false),
	    ack(),
	    true;
	Other ->
	    Other
    end.

safe_fixtable(Module, Tab, Bool) ->
    safe_fixtable(Module, Tab, Bool, self()).

%%%----------------------------------------------------------------------
%%% table_closed(Module, Tab) -> void()
%%% Module = dets | ets
%%% Tab = atom() | integer() | pid() (Table identifier for dets or ets table)
%%% Informs the fixtable server that a table has been closed. Only needed
%%% for ets tables. Dets tables are automatically cleaned up as the dets 
%%% process terminates on close.
%%%----------------------------------------------------------------------
table_closed(Module, Tab) ->
    (catch call_unchecked({{Module, Tab}, closed})),
    ok.

%%%----------------------------------------------------------------------
%%% table_closed(Module, Tab, Proc) -> void() 
%%% Module = dets | ets
%%% Tab = atom() | integer() | pid() (Table identifier for dets or ets table)
%%% Informs the fixtable server that a table has been closed by a process, 
%%% but not by all processes. Used by dets.
%%%----------------------------------------------------------------------
table_closed(Module, Tab, Proc) ->
    case (catch call_unchecked({{Module, Tab}, Proc, closed})) of 
	%%Errors in close are silently ignored
	{true, releaseit} ->                    
	    (catch Module:fixtable(Tab, false)),
	    ack();
	_ ->
	    true
    end.

%%%----------------------------------------------------------------------
%%% info(Module, Tab) -> [{Process, Refcount}] 
%%% Module = dets | ets
%%% Tab = atom() | integer() | pid() (Table identifier for dets or ets table)
%%% Process = pid()
%%% Refcount = integer()
%%% Returns a list of processes that holds fixations on the table
%%% and for each process a reference count.
%%%----------------------------------------------------------------------
info(Module, Tab) ->
    call({{Module, Tab}, info}).

%%%----------------------------------------------------------------------
%%% The server initialization
%%%----------------------------------------------------------------------
server_init(Starter) -> %% Starter is the supervisor_bridge
    case (catch register(?PROCNAME, self())) of
	%% Actually, this can no longer happen in full OTP, as i'm started
	%% by the kernel supervisor.
	{'EXIT', {badarg, _}} -> %% Parallel starting, we lost.
	    Starter ! {self(), false};
	true ->
	    Starter ! {self(), true},
	    process_flag(trap_exit,true),
	    loop(Starter);
	Other ->
	    exit({badarg, {?MODULE, init, [Starter]}})
    end.


%%%------------------------------------------------------------------------
%%% Here is the complete supervisor bridge, init, terminate and start_link.
%%%------------------------------------------------------------------------
init([]) -> % Called by supervisor_bridge:start_link
    Pid = spawn_link(?MODULE,server_init,[self()]),
    receive
	{Pid, false} ->
	    ignore;
	{Pid, _} ->
	    {ok, Pid, Pid}
    after ?CALL_TIMEOUT ->
	    {error, {timeout, ?PROCNAME}}
    end.

terminate(Reason,Pid) ->
    (catch exit(Pid,kill)),
    ok.

start_link() ->
    supervisor_bridge:start_link({local, ?PROCNAME_SUP}, ?MODULE, []).

%%-------------------------------------------------------------------------
%% Do the actual calls tho the server, start it if needed
%%-------------------------------------------------------------------------
call(Message) ->
    case whereis(?PROCNAME) of
	undefined ->
	    %%% Server _may_ not be started
	    ChildSpec = {?PROCNAME_SUP, {?MODULE, start_link, []},
			 permanent, 2000, supervisor, [?MODULE]},
	    %% This may result in {error,{already_started,Pid}}, but then
	    %% everything is OK.
	    (catch supervisor:start_child(kernel_sup, ChildSpec)), 
	    ?PROCNAME ! {self(), Message};
	_ ->
	    ?PROCNAME ! {self(), Message}
    end,
    receive 
	{?PROCNAME, Reply} ->
	    Reply
    after ?CALL_TIMEOUT ->
	    exit({timeout, {?MODULE, call, [Message]}})
    end.

%%%--------------------------------------------------------------
%%% Same as above, but dont try to start it. Only for close messages
%%%--------------------------------------------------------------
call_unchecked(Message) ->
    ?PROCNAME ! {self(), Message}, 
    receive 
	{?PROCNAME, Reply} ->
	    Reply
    after ?CALL_TIMEOUT ->
	    exit({timeout, {?MODULE, call, [Message]}})
    end.


%%%----------------------------------------------------------------------
%%% The server main loop
%%%----------------------------------------------------------------------
loop(Parent) -> %% Parent is the supervisor bridge.
    Processes = ets:new(fixtable_processes, [bag] ++ ?COMMON_TABLE_OPTIONS),
    Owners = ets:new(fixtable_owners, [bag] ++ ?COMMON_TABLE_OPTIONS),
    Tables = ets:new(fixtable_tables, [bag] ++ ?COMMON_TABLE_OPTIONS),
    loop(Parent, Processes, Owners, Tables).

loop(Parent, Processes, Owners, Tables) ->
    receive 
	{'EXIT', Parent, TerminateReason} -> %% Supervisor dead.
	    exit(TerminateReason);
	{'EXIT', Proc, _} ->
	    (catch handle_exit(Proc, Processes, Owners, Tables)),
	    loop(Parent, Processes, Owners, Tables);
	{system, From, Req} ->
	    sys:handle_system_msg(Req, From, Parent, ?MODULE, [], 
				  {Processes, Owners, Tables});
	{Proc, Request} ->
	    Reply = handle_request(Proc, Request, Processes, Owners, Tables),
	    case Reply of
		{sync, Rep} ->
		    Proc ! {?PROCNAME, Rep},
		    wait_ack(Proc);
		Rep ->
		    Proc ! {?PROCNAME, Rep}
	    end,
	    loop(Parent, Processes, Owners, Tables);
	Other ->
	    log(lists:flatten(io_lib:format("Got unhandled message ~p.",
					    [Other]))),
	    loop(Parent, Processes, Owners, Tables)
    end.

%%------------------------------------------------------------------
%% Actually handle the request
%%------------------------------------------------------------------
handle_request(Proc, Request, Processes, Owners, Tables) ->
    (catch handle_request2(Proc, Request, Processes, Owners, Tables)).	

handle_request2(_Caller, {Tab, true, Own, Proc}, Processes, Owners, Tables) ->
    %% Fix table
    %% Already fixed?
    Ret = case ets:lookup(Owners, Tab) of
	      [] ->
		  %% OK, first fix
		  insert_owner(Tab, Own, Owners),
		  {sync, {true, fixit}};
	      _ ->
		  true
	  end,
    case ets:lookup(Processes, Proc) of
	[] ->
	    case (catch link(Proc)) of
		{noproc, _} ->
		    throw({error, caller_dead});
		_ -> 
		    ok
	    end;
	_ ->
	    ok
    end,
    case ets:match_object(Processes,{Proc, Tab, '_'}) of
	[{Proc, Tab, N}] ->
	    ets:match_delete(Processes,{Proc,Tab,N}),
	    ets:insert(Processes, {Proc,Tab,N+1});
	[] ->
	    %% This is the first fix by this process
	    ets:insert(Processes, {Proc, Tab, 1}),
	    ets:insert(Tables, {Tab, Proc});
	_ ->
	    log("Inconsistent fixtable stucture")
    end,
    Ret;

handle_request2(_Caller, {Tab, false, Own, Proc}, 
		Processes, Owners, Tables) ->
    %% Release a fix.
    case ets:match_object(Processes, {Proc, Tab, '_'}) of
	[{Proc, Tab, 1}] ->
	    ets:match_delete(Tables,{Tab,Proc}),
	    Ret = case ets:lookup(Tables,Tab) of
		      [] ->
			  wipe_table_owner(Tab, Owners),
			  {sync, {true, releaseit}};
		      _ ->
			  true
		  end,
	    ets:match_delete(Processes, {Proc, Tab, '_'}),
	    Ret;
	[{Proc, Tab, NN}] ->
	    ets:match_delete(Processes, {Proc, Tab, NN}),
	    ets:insert(Processes, {Proc, Tab, NN-1}),
	    true;
	[] ->
	    false;
	_ ->
	    log("Inconsistent internal data structure"),
	    false
    end;

handle_request2(_Caller, {Tab, closed}, Processes, Owners, Tables) ->
    wipe_table(Tab, Processes, Owners, Tables),
    true;

handle_request2(_Caller, {Tab, Proc, closed}, Processes, Owners, Tables) ->
    case ets:match_object(Processes, {Proc, Tab, '_'}) of
	[{Proc, Tab, N}] ->
	    ets:match_delete(Tables,{Tab,Proc}),
	    Ret = case ets:lookup(Tables,Tab) of
		      [] ->
			  wipe_table_owner(Tab, Owners),
			  {sync, {true, releaseit}};
		      _ ->
			  true
		  end,
	    ets:match_delete(Processes, {Proc, Tab, '_'}),
	    Ret;
	[] ->
	    false;
	_ ->
	    log("Inconsistent internal data structure"),
	    false
    end;

handle_request2(_Caller, {Tab, info}, Processes, Owners, Tables) ->
    case ets:lookup(Owners, Tab) of
	[{Tab, Own, Tim}] ->
	    {Tim,lists:map(fun({P,T,R}) ->
				    {P,R}
			    end,
			    ets:match_object(Processes, {'_', Tab, '_'}))};
	X ->
	    false
    end;

handle_request2(Proc, Other, Processes, Owners, Tables) ->
    log({"Unknown request", Other}),
    {error, {badrequest,[Other]}}.
	    

handle_exit(Proc, Processes, Owners, Tables) ->    
    case ets:lookup(Owners,Proc) of
	[] ->
	    wipe_process(Proc, Processes, Owners, Tables);
	[{Proc, Tab}] ->
	    %% This is a table owner
	    wipe_table(Tab, Processes, Owners, Tables);
	_ ->
	    ok
    end.


%%------------------------------------------------------------------
%% System messages callbacks
%%------------------------------------------------------------------

system_continue(Parent, _, {Processes, Owners, Tables}) ->
    loop(Parent, Processes, Owners, Tables).

system_terminate(Reason, Parent, _, State) ->
    exit(Reason).

system_code_change(State, _Module, OldVsn, Extra) ->
    {ok, State}. %% Nothing to do in this version.


%%%------------------------------------------------------------------
%%% Internal helpers
%%%------------------------------------------------------------------
insert_owner(Table, Own, Owners) ->
    case (catch link(Own)) of
	{noproc, _} ->
	    throw({error, undefined});
	_ ->
	    ok
    end,
    ets:insert(Owners,{Own, Table}),
    ets:insert(Owners,{Table,Own, erlang:now()}).
    

wipe_table_owner(Table, Owners) ->
    case ets:lookup(Owners, Table) of
	[{Table, OwnerProc, _}] ->
	    ets:delete(Owners,OwnerProc),
	    ets:delete(Owners,Table);
	_ ->
	    ok
    end.

wipe_table(Tab, Processes, Owners, Tables) ->
    TabEntries = ets:match_object(Tables, {Tab, '_'}),
    lists:foreach(fun({T,P}) ->
			  ets:match_delete(Processes, {P,T,'_'})
		  end, TabEntries),
    ets:delete(Tables,Tab),
    wipe_table_owner(Tab, Owners).
			  

wipe_process(Proc, Processes, Owners, Tables) ->
    ProcEntries = ets:match_object(Processes, {Proc, '_', '_'}),
    lists:foreach(fun({P,T,N}) ->
			  ets:match_delete(Tables,{T,P}),
			  case ets:lookup(Tables,T) of
			      [] ->
				  wipe_table_owner(T, Owners),
				  %%% No more locks, try to
				  %%% release the lock ourselves 
				  %%% (this could only happen on public tables)
				  {Module, Tid} = T,
				  (catch Module:fixtable(Tid, false));
			      _ ->
				  ok
			  end
		  end,
		  ProcEntries),
    ets:delete(Processes, Proc).

log(String) ->
    error_logger:error_report({?MODULE, group_leader(), String}).
	
wait_ack(Pid) ->
    receive 
	{Pid, ack} ->
	    true;
	{'EXIT', Pid, Reason} -> % Died while we are waiting, 
				 % pass it on to the main loop...
	    self() ! {'EXIT', Pid, Reason},
	    true
    after ?CALL_TIMEOUT ->
	    log("Timeout reading ack from process fixing table!")
    end.

ack() -> 
   ?PROCNAME ! {self(), ack}. 

