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
-module(eva_log_snmp).

-behaviour(gen_server).

%% External exports
-export([create_tables/1, start/2, start_link/2]).
-export([eva_log_discr_table/1, eva_log_discr_table/3,
	 eva_log_total_max_size/1, eva_log_total_max_allowed/1,
	 log_filter/2]).
-export([eva_log_discr_try_change_status/4]).
-export([eva_log_type/2, eva_log_type/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_info/2, terminate/2]).

%%%-----------------------------------------------------------------
%%% General
%%% -------
%%% This module implements an SNMP interface towards eva_log. This
%%% interface is defined in OTP-EVA-LOG-MIB, and the instrumentation
%%% functions for that MIB is implemented here, as well as a server
%%% eva_log_snmp.  The MIB allows for managers to create logs.  The
%%% definition of these logs are stored in the persistent Mnesia
%%% tables logTable and evaLogDiscriminatorTable.  The owner of these
%%% tables is the server (eva_log_snmp).
%%%
%%% Restart behaviour
%%% -----------------
%%% This server may very well be locally or globally (failover)
%%% restarted without affecting any other part of the EVA
%%% functionality.  When it initialises itself, it rebuilds its
%%% internal state from the Mnesia tables and the state of eva_log.
%%% When it starts, it compares the persistently stored info with what
%%% eva_log knows, and recreates logs of necessary. Thus, if eva_log
%%% crashes, this server must restart as well.
%%%
%%% When performing a takeover, no special state transfer is needed.
%%% The server rebuilds its state from the Mnesia tables, not from
%%% data within the old server.
%%%-----------------------------------------------------------------
-include("eva.hrl").
-include("eva_log.hrl").
-include("OTP-EVA-LOG-MIB.hrl").
-include_lib("snmp/include/SNMPv2-TC.hrl").
-include("OTP-LOG-MIB.hrl").
-include("log_snmp.hrl").
-include("log.hrl").

-record(state, {log_dir, max_allowed, expire = 0, mibs}).

-record(evaLogDiscriminatorTable, {key, evaLogDiscrRowStatus}).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
create_tables(Nodes) ->
    {atomic, ok} =
	mnesia:create_table(
	  [{name, evaLogDiscriminatorTable},
	   {attributes, record_info(fields, evaLogDiscriminatorTable)},
	   {snmp, [{key, {integer, integer}}]},
	   {disc_copies, Nodes}]),
    ok.

start(LogDir, MaxSize) -> % tmp
    gen_server:start({local, eva_log_snmp}, eva_log_snmp,
		     {LogDir,  MaxSize}, []).

%%-----------------------------------------------------------------
%% Func: start_link(LogDir, MaxSize)
%% Types: LogDir = string()
%%        MaxSize = int() > 0
%% Purpose: The Mneisa tables logTable and evaLogDiscriminatorTable
%%          are replicated (on disk) on Nodes.  LogDir is where the
%%          manager-created logs should be created, and MaxSize is
%%          the maximum size these logs may occupy. This size should
%%          be set with the size of the actual partition in mind.
%%-----------------------------------------------------------------
start_link(LogDir, MaxSize)
  when list(LogDir), integer(MaxSize), MaxSize > 0 -> 
    gen_server:start_link({local, eva_log_snmp}, eva_log_snmp,
			  {LogDir, MaxSize}, []).

%% Internal API
get_log_dir() ->
    gen_server:call(eva_log_snmp, get_log_dir).

get_total_max_allowed() ->
    gen_server:call(eva_log_snmp, get_total_max_allowed).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------
init({LogDir, MaxSize}) ->
    process_flag(trap_exit, true),
    case mnesia:force_load_table(evaLogDiscriminatorTable) of
	yes ->
	    Priv = code:priv_dir(eva),
	    Mibs = [Priv ++ "/mibs/OTP-EVA-LOG-MIB"],
	    ok = snmp:load_mibs(snmp_master_agent, Mibs),
	    log_snmp:register_type(eva_log, ?evaLogType,
				   {?MODULE, eva_log_type, []}),
	    check_logs(LogDir),
	    {ok, #state{log_dir = LogDir, max_allowed = MaxSize, mibs = Mibs}};
	{error, R} ->
	    {stop, {mnesia_tables_not_available, [evaLogDiscriminatorTable], R}}
    end.

handle_call(get_log_dir, _From, S) ->
    {reply, S#state.log_dir, S};

handle_call(get_total_max_allowed, _From, S) ->
    {reply, S#state.max_allowed, S}.

handle_info(Info, S) ->
    {noreply, S}.

terminate(Reason, S) ->
    snmp:unload_mibs(snmp_master_agent, S#state.mibs).

%%%-----------------------------------------------------------------
%%% Instrumentation functions 
%%%-----------------------------------------------------------------
eva_log_total_max_size(get) ->
    {value, total_max_size()};
eva_log_total_max_size(_) ->
    ok.

eva_log_total_max_allowed(get) ->
    {value, get_total_max_allowed()};
eva_log_total_max_allowed(_) ->
    ok.

%%-----------------------------------------------------------------
%% When a modification to evaLogDiscriminatorTable is made, it means
%% that a row is either added or deleted.  When this happens, we
%% must reconfigure the eva_log filter for the log.
%%-----------------------------------------------------------------
eva_log_discr_table(Op) ->
    ok.
eva_log_discr_table(is_set_ok, RowIndex, Cols) ->
    snmp_generic:table_try_row({evaLogDiscriminatorTable, mnesia}, 
			       {?MODULE, eva_log_discr_try_change_status},
			       RowIndex, Cols);
eva_log_discr_table(set, RowIndex, Cols) ->    
    %% reconfigure log - make new filter
    case snmp_generic:table_func(set, RowIndex, Cols,
				 {evaLogDiscriminatorTable, mnesia}) of
	{noError, 0} ->
	    [LogIndex, _] = RowIndex,
	    [#logTable{logName = Name}] = 
		mnesia:dirty_read({logTable, LogIndex}),
	    Events = get_event_indexes(LogIndex),
	    eva_log:set_filter(Name, {?MODULE, log_filter, [Events]}),
	    {noError, 0};
	Error ->
	    Error
    end;
eva_log_discr_table(Op, RowIndex, Cols) ->
    snmp_generic:table_func(Op, RowIndex, Cols,
			    {evaLogDiscriminatorTable,mnesia}).

eva_log_discr_try_change_status(_, ?'RowStatus_createAndGo', [LogIndex,_], _) ->
    case mnesia:dirty_read({logTable, LogIndex}) of
	[#logTable{logOwner = ?AgentOwner}] ->
	    %% It is not possible to modify the log discrimination for
	    %% system-created logs. 
	    {wrongValue, ?evaLogDiscrRowStatus};
	[] -> % The log must exist
	    {inconsistentValue, ?evaLogDiscrRowStatus};
	_ -> % Otherwise its ok!
	    {noError, 0}
    end;
eva_log_discr_try_change_status(_, ?'RowStatus_destroy', [LogIndex, _], _) ->
    case mnesia:dirty_read({logTable, LogIndex}) of
	[#logTable{logOwner = ?AgentOwner}] ->
	    {wrongValue, ?evaLogDiscrRowStatus};
	_ ->
	    {noError, 0}
    end;
eva_log_discr_try_change_status(_, _, _, _) ->
    {wrongValue, ?evaLogDiscrRowStatus}.

%%-----------------------------------------------------------------
%% Filter predicate for operator-defined logs.
%%-----------------------------------------------------------------
log_filter(#event{name = Name}, EventIndexes) ->
    {ok, Index} = eva_snmp_adaptation:name2index(Name),
    lists:member(Index, EventIndexes);
log_filter(#alarm{name = Name}, EventIndexes) ->
    {ok, Index} = eva_snmp_adaptation:name2index(Name),
    lists:member(Index, EventIndexes);
log_filter(_X, _) ->
    false.

%%-----------------------------------------------------------------
%% Functions called by LOG when logTable is modified
%%-----------------------------------------------------------------
eva_log_type(create, Log) ->
    mk_log(Log, get_log_dir());

eva_log_type(delete, Log) ->
    %% delete all discriminators
    #logTable{logIndex = LogIndex, logName = Name} = Log,
    Events = get_event_indexes(LogIndex),
    lists:foreach(fun(EventIndex) ->
			  mnesia:dirty_delete({logDiscriminatorTable,
					       {LogIndex, EventIndex}})
		  end, Events),
    eva_log:close(Name).

eva_log_type(validate_creation, LogIndex, Cols) ->
    {Max, Col} = case lists:keysearch(?logMaxSize, 1, Cols) of
		     {value, {_, M}} -> {M, ?logMaxSize};
		     _ -> {?default_logMaxSize, ?logRowStatus}
		 end,
    TotalMaxSize = total_max_size(),
    MaxAllowed = get_total_max_allowed(),
    if
	Max + TotalMaxSize =< MaxAllowed ->
	    true;
	true ->
	    {inconsistentValue, Col}
    end;

eva_log_type(search, LogIndex, LogTrIndex) ->
    [#logTable{logName = Name}] = mnesia:dirty_read({logTable, LogIndex}),
    [#logTransferTable{logTransferStartTime = Start,
		       logTransferStopTime = Stop}] =
	mnesia:dirty_read({logTransferTable, {LogIndex, LogTrIndex}}),
    {eva_log, eva_log_search, [Name, mk_start(Start), mk_stop(Stop)]}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
mk_start(undefined) -> {{1970,1,1},{12,0,0}};
mk_start(Date) -> snmp:date_and_time_to_universal_time(Date).

mk_stop(undefined) -> calendar:universal_time();
mk_stop(Date) -> snmp:date_and_time_to_universal_time(Date).

total_max_size() ->
    total_max_size(mnesia:snmp_get_next_index(logTable, []), 0).

total_max_size({ok, [Index]}, Sum) ->
    case mnesia:dirty_read({logTable, Index}) of
	[Log] when Log#logTable.logType == ?evaLogType ->
	    total_max_size(mnesia:snmp_get_next_index(logTable, [Index]),
			   Sum + Log#logTable.logMaxSize);
	_ ->
	    total_max_size(mnesia:snmp_get_next_index(logTable, [Index]),
			   Sum)
    end;
total_max_size(endOfTable, Sum) -> Sum.

mk_log(Log, LogDir) ->
    #logTable{logIndex = LogIndex, logName = Name,
	      logAdminStatus = Admin, logMinWrapTime = WrapTime,
	      logMaxSize = Size, logWrapPercentage = WrapPer} = Log,
    MaxF = 100 div WrapPer,
    MaxB = Size div MaxF,
    %% Open the disk file...
    case disk_log:open([{name, Name},
			{file, filename:join(LogDir, Name) ++ ".LOG"},
			{type, wrap},
			{size, {MaxB, MaxF}}]) of
	{ok, Name} ->
	    Events = get_event_indexes(LogIndex),
	    WrapSecs = WrapTime * 3600,
	    %% ... and make sure that all events&alarms get sent to the log.
	    %% (it will make sure the log shows up in the logTable)
	    eva_log:open(Name, {?MODULE, log_filter, [Events]}, WrapSecs),
	    ok;
	Error ->
	    error_logger:error_msg("eva_log_snmp: disk_log:open failed ~w\n",
				   [Error]),
	    error
    end.

get_event_indexes(LogIndex) ->
    Pat0 = mnesia:table_info(evaLogDiscriminatorTable, wild_pattern),
    Pat = Pat0#evaLogDiscriminatorTable{key = {LogIndex, '_'}},
    Fun = fun() ->
		  [element(2, Log#evaLogDiscriminatorTable.key)
		   || Log <- mnesia:match_object(Pat)]
	  end,
    mnesia:ets(Fun).

%%-----------------------------------------------------------------
%% Loop through all logs persistently stored in logTable.  For
%% each eva_log, check if there is a corresponding log created in log.
%% If it is, everything is ok.  If not, check if the log was
%% created by someone else.  If it was, set the logOperStatus to down,
%% otherwise, we have created the log, so we must recreate it.  If
%% there are logs in the log that are not in LogTable, insert
%% them.
%%-----------------------------------------------------------------
check_logs(LogDir) ->
    check_logs(mnesia:dirty_first(logTable), log:get_logs(), LogDir).

check_logs('$end_of_table', Logs, LogDir) ->
    ok;
check_logs(Index, Logs, LogDir) ->
    [Log] = mnesia:dirty_read({logTable, Index}),
    #logTable{logName = Name, logType = Type, logOwner = Owner} = Log,
    case lists:keysearch(Name, #log.name, Logs) of
	{value, _} ->
	    %% The log exists already
	    ok;
	_ when Type == ?evaLogType, Owner /= ?AgentOwner ->
	    %% This is an EVA log created by a manager.  We must
	    %% recreate it and set its status to up.
	    mk_log(Log, LogDir),
	    log:set_admin_status(Name, up);
	_ ->
	    %% Otherwise, the log is created by some other application.
	    %% It is up to that application to recreate the log.
	    ok
    end,
    NLogs = lists:keydelete(Name, #log.name, Logs), 
    check_logs(mnesia:dirty_next(logTable, Index), NLogs, LogDir).

    
