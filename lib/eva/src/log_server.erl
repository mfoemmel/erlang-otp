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
-module(log_server).

-behaviour(gen_server).

-include("log.hrl").
-include("eva.hrl").

%% TODO
%% - must handle logs on other nodes

-record(state, {logs = [], wraps = []}).
%%-----------------------------------------------------------------
%% logs  = [#log]   - a list of all logs known to the log server
%% wraps = [{LogName, LastWrapTime, Fault}] - a list of when the
%%             logs wrapped.  If an alarm was set, the fault id is
%%             stored in Fault, otherwise Fault is 'false'.
%%-----------------------------------------------------------------

%% External exports
-export([start_link/0, start/0]).

%% Internal exports
-export([init/1, handle_call/3, handle_info/2, terminate/2]).

%%%-----------------------------------------------------------------
%%% General
%%% -------
%%% This module implements a generic log functionality.  The server
%%% controls the log processes, which are ordinary disk_log
%%% processes.  The application logs to these processes directly.
%%%
%%% The interface to this server is in the module 'log'.
%%%
%%% Restart behaviour
%%% -----------------
%%% This server has no persistent state, i.e.  the internal state is
%%% lost during failover in a distributed system, or when the
%%% process/subsystem restarts.  The state is built from calls to
%%% open/2, set_*/2 and close/1, and it is up to the caller of these
%%% functions (i.e. the owner of the logs) to call them again after
%%% failover, takeover or restart.  The reason for this is that
%%% failover behavoiur must be configurable.  Some logs may be
%%% replicated on several nodes, so log info is not lost during
%%% failover.  Other logs are not replicated, and log items are copied
%%% on takeover, and for other logs, the log items are deleted on
%%% takeover.  This is entirely up to the owner of the log.  This
%%% means that log-owners may want to monitor the behaviour of this
%%% server, either directly by linking to it, or indirectly by making
%%% sure that the supervisor restarts the owner if this process
%%% terminates.
%%%
%%% When performing a takeover, no special state transfer is needed,
%%% as the server has no 'real' state.
%%%
%%% OAM functionality used
%%% ----------------------
%%% eva_server
%%%-----------------------------------------------------------------

%%%-----------------------------------------------------------------
%%% API
%%%-----------------------------------------------------------------
start() ->
    gen_server:start({local, log_server}, log_server, [], []).

start_link() ->
    gen_server:start_link({local, log_server}, log_server, [], []).

%%%-----------------------------------------------------------------
%%% Call-back functions from gen_server
%%%-----------------------------------------------------------------
init(_) ->
    eva:register_alarm(log_wrap_too_often, true, processing, major),
    eva:register_alarm(log_file_error, true, processing, critical),
    Alarms = eva:get_alarms({name, log_wrap_too_often}) ++
	     eva:get_alarms({name, log_file_error}),
    lists:foreach(fun(#alarm{fault_id = FaultId}) ->
			  eva:clear_alarm(FaultId)
		  end, Alarms),
    {ok, #state{}}.

handle_call(get_logs, _From, S) ->
    {reply, S#state.logs, S};

handle_call({open, Name, Type, WrapT}, _From, S) ->
    case disk_log_open(Name) of
	{ok, Name} ->
	    Log = #log{name = Name, type = Type, wrapt = WrapT},
	    {reply, ok, S#state{wraps = [{Name, 0, false} | S#state.wraps],
				logs = [Log | S#state.logs]}};
	Error ->
	    {reply, Error, S}
    end;

handle_call({set_admin_status, Name, Status}, _From, S) ->
    case lists:keysearch(Name, #log.name, S#state.logs) of
	{value, Log} ->
	    {Oper, NLog} = upd(Log, Status),
	    {reply, Oper, update(S, NLog)};
	_ ->
	    {reply, {error, {no_such_log, Name}}, S}
    end;

handle_call({close, Name}, _From, S) ->
    disk_log:close(Name),
    {reply, ok, S#state{logs = lists:keydelete(Name, #log.name,S#state.logs)}}.


handle_info({disk_log, Node, LogName, {wrap, Lost}}, S) ->
    case lists:keysearch(LogName, 1, S#state.wraps) of
	{value, {_, LastWrap, Fault}} ->
	    {value, #log{wrapt = MinWrapT}} =
		lists:keysearch(LogName, #log.name, S#state.logs),
	    Now = get_time(),
	    NWrap =
		if
		    LastWrap + MinWrapT > Now, Lost > 0, Fault == false ->
			%% New alarm condition
			FaultId = eva:get_fault_id(),
			eva:send_alarm(log_wrap_too_often, FaultId,
				       LogName, [], []),
			{LogName, Now, {true, FaultId}};
		    LastWrap + MinWrapT > Now, Lost > 0 ->
			%% Same alarm still active, do not report this again
			{LogName, Now, Fault};
		    Fault == false ->
			%% We didn't loose anything, no active alarm
			{LogName, Now, false};
		    true ->
			%% We didn't loose anything, clear active alarm
			{true, FaultId} = Fault,
			eva:clear_alarm(FaultId),
			{LogName, Now, false}
		end,
	    NWraps = lists:keyreplace(LogName, 1, S#state.wraps, NWrap),
	    {noreply, S#state{wraps = NWraps}};
	_ ->
	    {noreply, S}
    end;

handle_info({disk_log, Node, LogName, {error_status, Status}}, S) ->
    NLogs = lists:map(
	      fun(Log) when Log#log.name == LogName ->
		      chk(Status, Log);
		 (Log) ->
		      Log
	      end, S#state.logs),
    {noreply, S#state{logs = NLogs}};

handle_info(_, S) ->
    {noreply, S}.


terminate(R, _State) ->
    ok.

%%%-----------------------------------------------------------------
%%% Internal functions
%%%-----------------------------------------------------------------
upd(Log, Status) when Log#log.oper_status == up,
		      Status == down ->
    disk_log:block(Log#log.name, false),
    {down, Log#log{admin_status = down, oper_status = down}};
upd(Log, Status) when Log#log.oper_status == down,
		      Status == up ->
    disk_log:unblock(Log#log.name),
    {up, Log#log{admin_status = up, oper_status = up}};
upd(Log, Status) when tuple(Log#log.oper_status),
		      Status == down ->
    {down, Log#log{admin_status = down, oper_status = down}};
upd(Log, Status) ->
    {Log#log.oper_status, Log}.


chk(ok, Log) ->
    case Log#log.oper_status of
	{down, FaultId} when Log#log.admin_status == up ->
	    eva:clear_alarm(FaultId),
	    Log#log{oper_status = up};
	{down, FaultId} ->
	    eva:clear_alarm(FaultId),
	    Log#log{oper_status = down};
	_ -> Log
    end;
chk({error, Error}, Log) ->
    case Log#log.oper_status of
	up ->
	    FaultId = eva:get_fault_id(),
	    eva:send_alarm(log_file_error, FaultId, Log, Error, []),
	    Log#log{oper_status = {down, FaultId}};
	_ ->
	    Log
    end;
chk(_, Log) ->
    Log.

update(S, Log) ->
    Logs = lists:keyreplace(Log#log.name, #log.name, S#state.logs, Log),
    S#state{logs = Logs}.

disk_log_open(Name) ->
    case disk_log:info(Name) of
	L when list(L) ->
	    {value, {_, File}} = lists:keysearch(file, 1, L),
	    disk_log:open([{name, Name}, {notify, true}, {file, File}]);
	Error ->
	    Error
    end.

get_time() ->
    {M,S,_U} = now(),
    1000000 * M + S.
