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
-module(log_snmp).

%% External exports
-export([create_tables/1, start_link/0, register_type/3]).

-export([log_file_error/1, log_wrap_too_often/1, log_table/1, log_table/3,
	 log_table_try_change_status/4, log_table_change_status/4]).
-export([log_tr_table/1, log_tr_table/3,
	 log_tr_table_try_change_status/4, log_tr_table_change_status/4]).

%% Internal exports
-export([init/1, handle_call/3, handle_info/2, terminate/2]).
-export([transfer/2, default_log_search/2]).

%%%-----------------------------------------------------------------
%%% General
%%% -------
%%% This module implements an SNMP interface towards log. This
%%% interface is defined in OTP-LOG-MIB, and the instrumentation
%%% functions for that MIB is implemented here, as well as a server
%%% log_snmp.  The MIB allows for managers to create logs and transfer
%%% logs via FTP.  The definition of these logs are stored in the
%%% persistent Mnesia tables logTable and logTransferTable.  The owner
%%% of these tables is the server (log_snmp).
%%%
%%% Restart behaviour
%%% -----------------
%%% This server may very well be locally or globally (failover)
%%% restarted without affecting any other part of the LOG
%%% functionality.  When it initialises itself, it rebuilds its
%%% internal state from the Mnesia tables and the state of log.
%%% When it starts, it compares the persistently stored info with what
%%% log_server knows, and recreates logs of necessary. Thus, if
%%% log_server crashes, this server must restart as well.
%%%
%%% When performing a takeover, no special state transfer is needed.
%%% The server rebuilds its state from the Mnesia tables, not from
%%% data within the old server.
%%%
%%% OAM functionality used
%%% ----------------------
%%% log_server
%%%-----------------------------------------------------------------
-include("eva.hrl").
-include_lib("snmp/include/SNMPv2-TC.hrl").
-include("OTP-LOG-MIB.hrl").
-include("log_snmp.hrl").
-include("log.hrl").

-define(interval, 5000).

-record(state, {expire = 0, types = [], sessions = [], services = []}).
%%-----------------------------------------------------------------
%% expire   = integer() - Keeps track of when the cache data expires
%% types    = [{Type, TypeOid, TypeFunc}] - maps each log type to the
%%                         corresponding OID and function associated
%%                         with the type of log.
%% sessions = [{pid(), Index}] - A list of all active transfer
%%                         sessions and their corresponding row.
%% services = [service()] - All other services that we have
%%                          initialised, and must clean up when we stop.
%%                          E.g. log_snmpea, log_httpd etc.
%%-----------------------------------------------------------------

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
create_tables(Nodes) ->
    {atomic, ok} =
	mnesia:create_table([{name, logTable},
			     {attributes, record_info(fields, logTable)},
			     {snmp, [{key, integer}]},
			     {disc_copies, Nodes}]),
    {atomic, ok} =
	mnesia:create_table([{name, logTransferTable},
			     {attributes, record_info(fields,logTransferTable)},
			     {snmp, [{key, {integer, integer}}]},
			     {disc_copies, Nodes}]),
    ok.

start_link() ->
    gen_server:start_link({local, log_snmp}, log_snmp, [], []).

register_type(Type, TypeOid, {M,F,A}) 
  when list(TypeOid), atom(M), atom(F), list(A) ->
    gen_server:call(log_snmp, {register_type, Type, TypeOid, {M,F,A}}).

%% Internal API
check_cache() ->
    gen_server:call(log_snmp, check_cache).

invalidate_cache() ->
    gen_server:call(log_snmp, invalidate_cache).

get_type_func(TypeOid) ->
    gen_server:call(log_snmp, {get_type_func, TypeOid}).

activate_transfer(RowIndex) ->
    gen_server:call(log_snmp, {activate_transfer, RowIndex}).

deactivate_transfer(RowIndex) ->
    gen_server:call(log_snmp, {deactivate_transfer, RowIndex}).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_server
%%%----------------------------------------------------------------------
init(_) ->
    process_flag(trap_exit, true),
    case mnesia:force_load_table(logTable) of
	yes ->
	    yes = mnesia:force_load_table(logTransferTable),
	    check_logs(),
	    Alarms = [{log_wrap_too_often, logWrapAlarm, snmpTrap,
		       "standard trap", {?MODULE, log_wrap_too_often}},
		      {log_file_error, logMediaErrorAlarm, snmpTrap,
		       "standard trap", {?MODULE, log_file_error}}],
	    Priv = code:priv_dir(eva),
	    Mibs = [Priv ++ "/mibs/OTP-LOG-MIB"],
	    snmp:load_mibs(snmp_master_agent, Mibs),
	    eva_snmp_adaptation:register_alarms(Alarms),
	    Services = init_services(),
	    {ok, #state{services = Services}};
	{error, R} ->
	    {stop, {mnesia_tables_not_available, [logTable], R}}
    end.

handle_call(check_cache, _From, S) ->
    case get_time() of
	CurrentTime when CurrentTime > S#state.expire ->
	    update_log_table(S#state.types),
	    {reply, ok, S#state{expire = CurrentTime + ?interval}};
	_ ->
	    {reply, ok, S}
    end;

handle_call(invalidate_cache, _From, S) ->
    {reply, ok, S#state{expire = 0}};

handle_call({get_type_func, TypeOid}, _From, S) ->
    Reply = get_type_func_i(TypeOid, S#state.types),
    {reply, Reply, S};

handle_call({register_type, Type, TypeOid, TypeFunc}, _From, S) ->
    NTypes = [{Type, TypeOid, TypeFunc} |
	      lists:keydelete(Type, 1, S#state.types)],
    {reply, ok, S#state{types = NTypes}};

handle_call({activate_transfer, RowIndex}, _From, S) ->
    Index = lidx(RowIndex),
    [TR] = mnesia:dirty_read({logTransferTable, Index}),
    [LogIndex, LogTrIndex] = RowIndex,
    case mnesia:dirty_read({logTable, LogIndex}) of
	[#logTable{logType = Type, logName = Name}] ->
	    {M,F,A} = case get_type_func_i(Type, S#state.types) of
			  {ok, Func} -> Func;
			  _Error -> {?MODULE, default_log_search, [Name]}
		      end,
	    %% Do not use proc_lib, because the exit reason
	    %% is used to indicate error.
	    Pid = spawn_link(?MODULE, transfer, [TR, {M,F,A}]),
	    Sessions = S#state.sessions,
	    {reply, ok, S#state{sessions = [{Pid, Index} | Sessions]}};
	_ ->
	    error_logger:error_msg("~w: log ~w does not exist in logTable~n",
				   [?MODULE, LogIndex]),
	    {reply, error, S}
    end;

handle_call({deactivate_transfer, RowIndex}, _From, S) ->
    Index = lidx(RowIndex),
    Sessions = S#state.sessions,
    case lists:keysearch(Index, 2, Sessions) of
	{value, {Pid, _}} ->
	    exit(Pid, abort),
	    [TR] = mnesia:dirty_read({logTransferTable, Index}),
	    mnesia:dirty_write(TR#logTransferTable{
					   logTransferLastResult = 
					   ?logTransferLastResult_aborted});
	_ ->
	    ok
    end,
    {reply, ok, S#state{sessions = lists:keydelete(Index, 2, Sessions)}}.


handle_info({'EXIT', Pid, Reason}, S) ->
    Sessions = S#state.sessions,
    case lists:keysearch(Pid, 1, Sessions) of
	{value, {_, Index}} ->
	    case Reason of
		ok ->
		    done(Index);
		{error, ftp_bad_address} ->
		    error(Index, ?logTransferLastResult_ftpBadAddress);
		{error, ftp_login_error} ->
		    error(Index, ?logTransferLastResult_ftpLoginError);
		{error, ftp_write_error} ->
		    error(Index, ?logTransferLastResult_ftpWriteError);
		_ ->
		    error(Index, ?logTransferLastResult_otherError)
	    end;
	_ ->
	    ok
    end,
    {noreply, S#state{sessions = lists:keydelete(Pid, 1, Sessions)}};

handle_info(Info, S) ->
    {noreply, S}.

terminate(Reason, S) ->
    lists:foreach(fun({Pid, _}) -> exit(Pid, abort) end, S#state.sessions),
    stop_services(S#state.services),
    snmp:unload_mibs(snmp_master_agent, ["OTP-LOG-MIB"]).
    

%%%-----------------------------------------------------------------
%%% Instrumentation functions 
%%%-----------------------------------------------------------------
%%-----------------------------------------------------------------
%% The logTable is implemented as an "cache" table.  When a get* is
%% performed, the cache is updated with real data if it was more than
%% ?interval msecs since the last update.  The real data that is
%% interesting is the logNumberOfRecords and logs created in log
%% by other subsystems (e.g. through an WWW-manager).  This means that
%% updates in the logs may not be visible for ?interval msecs.
%%-----------------------------------------------------------------
%% NOTE:
%% In this first version, we do not support modification of any other
%% attributes than logAdminStatus and logRowStatus.  The others must
%% be set at creation.
%%-----------------------------------------------------------------
log_table(Op) ->
    ok.
log_table(is_set_ok, RowIndex, UCols) ->
    %% Add initial values for read-only columns
    Cols = lists:keysort(1, [{?logOperStatus, ?logOperStatus_up},
			     {?logNumberOfRecords, 0} | UCols]),
    case lists:keysearch(?logRowStatus, 1, Cols) of
	{value, _} ->
	    snmp_generic:table_try_row({logTable,mnesia},
				       {?MODULE, log_table_try_change_status},
				       RowIndex,Cols);
	_ ->
	    %% It's only possible to modify status right now...
	    case UCols of
		[{?logAdminStatus, _}] -> {noError, 0};
		[{?logAdminStatus, _}, {Col, _}|_] -> {inconsistentValue, Col};
		[{Col, _} |_] -> {inconsistentValue, Col}
	    end
    end;
log_table(set, RowIndex, UCols) ->
    %% Add initial values for read-only columns
    Cols = lists:keysort(1, [{?logOperStatus, ?logOperStatus_up},
			     {?logNumberOfRecords, 0} | UCols]),
    [LogIndex] = RowIndex,
    case UCols of
	[{?logAdminStatus, Admin}] ->
	    [Log] = mnesia:dirty_read({logTable, LogIndex}),
	    #logTable{logName = Name} = Log,
	    Oper = case set_status(Name, Admin) of
		       up -> ?logOperStatus_up;
		       down -> ?logOperStatus_down
		   end,
	    NLog = set_log_status(Log, Admin, Oper),
	    mnesia:dirty_write(NLog),
	    {noError, 0};
	_ ->
	    case catch mnesia:async_dirty(
			 fun() ->
				 snmp_generic:table_set_row(
				   {logTable, mnesia},
				   {?MODULE, log_table_change_status},
				   nofunc, RowIndex, Cols)
			 end) of
		{'EXIT', _Reason} ->
		    {Col, _Val} = hd(UCols),
		    {commitFailed, Col};
		Value ->
		    Value
	    end
    end;
log_table(undo, [LogIndex], Cols) ->
    {noError, 0};
log_table(Op, RowIndex, Cols) ->
    check_cache(),
    snmp_generic:table_func(Op, RowIndex, Cols, {logTable, mnesia}).

%% Called in is_set_ok if logRowStatus is changed
log_table_try_change_status(_, ?'RowStatus_createAndGo', [LogIndex], Cols) ->
    %% It is not possible to create a new log with the same name as another
    {value, {_, Name}} = lists:keysearch(?logName, 1, Cols),
    case mnesia:dirty_match_object(?wildLogTable#logTable{logName = Name}) of
	[] ->
	    {value, {_, LogType}} = lists:keysearch(?logType, 1, Cols),
	    validate_creation(LogType, LogIndex, Cols);
	_Found ->
	    {noCreation, ?logName}
    end;
log_table_try_change_status(_, ?'RowStatus_destroy', _, _) ->
    {noError, 0};
log_table_try_change_status(_, _, _, _) ->
    {wrongValue, ?logRowStatus}.

%% Called in set if logRowStatus is changed
log_table_change_status(_, ?'RowStatus_createAndGo', [LogIndex], _) ->
    [Log] = mnesia:read({logTable, LogIndex}),
    case create_log(Log) of
	ok ->
	    {noError, 0};
	error ->
	    {commitFailed, ?logRowStatus}
    end;
log_table_change_status(_, ?'RowStatus_destroy', [LogIndex], _) ->
    Log = mnesia:read({logTable, LogIndex}),
    #logTable{logType = Type, logName = Name} = Log,
    case get_type_func(Type) of
	{ok, {M,F,A}} ->
	    catch apply(M, F, [delete, Log | A]);
	_ -> ok
    end,
    log:close(Name),
    disk_log:close(Name),
    {noError, 0}.



valiate_time(Cols) ->
    valid_time(Cols, ?logTransferStartTime),
    valid_time(Cols, ?logTransferStopTime).

valid_time(Cols, Col) ->
    case lists:keysearch(Col, 1, Cols) of
	{value, {_, DateAndTime}} ->
	    case snmp:validate_date_and_time(DateAndTime) of
		true -> ok;
		false -> throw({wrongValue, Col})
	    end;
	_ -> ok
    end.
    

log_tr_table(Op) ->
    ok.
log_tr_table(is_set_ok, RowIndex, UCols) ->    
    %% Add initial values for read-only columns
    LR = {?logTransferLastResult, ?logTransferLastResult_ok},
    Cols = lists:keysort(1, [LR | UCols]),
    case catch valiate_time(UCols) of
	ok ->
	    snmp_generic:table_try_row({logTransferTable, mnesia},
				       {?MODULE,log_tr_table_try_change_status},
				       RowIndex, Cols);
	Else -> Else
    end;
log_tr_table(is_set_ok, _RowIndex, [{Col,_} |_]) ->
    {noCreation, Col};

log_tr_table(set, RowIndex, UCols) ->
    Cols = 
	case mnesia:dirty_read({logTransferTable, lidx(RowIndex)}) of
	    [_Found] ->
		UCols;
	    _ ->
		%% Add initial values for read-only columns and non-snmp columns
		LR = {?logTransferLastResult, ?logTransferLastResult_ok},
		NS = {10, undefined},
		lists:keysort(1, [LR, NS | UCols])
	end,
    case catch mnesia:async_dirty(
		 fun() ->
			 snmp_generic:table_set_row(
			   {logTransferTable, mnesia},
			   {?MODULE, log_tr_table_change_status},
			   {snmp_generic_mnesia, table_try_make_consistent},
			   RowIndex,
			   Cols)
		 end) of
	{'EXIT', _Reason} ->
	    {Col, _Val} = hd(UCols),
	    {commitFailed, Col};
	Value ->
	    Value
    end;
log_tr_table(undo, RowIndex, Cols) ->
    {noError, 0};
log_tr_table(get, RowIndex, Cols) ->
    case snmp_generic:table_func(get,RowIndex,Cols,{logTransferTable,mnesia}) of
	Res when list(Res) ->
	    hide_passwd(Cols, Res);
	Else ->
	    Else
    end;
log_tr_table(get_next, RowIndex, Cols) ->
    case snmp_generic:table_func(get_next,RowIndex,Cols,
				 {logTransferTable,mnesia}) of
	Res when list(Res) ->
	    hide_passwd(Res);
	Else ->
	    Else
    end.

hide_passwd([?logTransferFTPPasswd|T], [{value, _Passwd} | Vals]) ->
    [{value, ""} | Vals];
hide_passwd([_|T], [Val | Vals]) ->
    [Val | hide_passwd(T, Vals)];
hide_passwd([], []) ->
    [].

hide_passwd([{[?logTransferFTPPasswd|Index], _Passwd} | T]) ->
    [{[?logTransferFTPPasswd|Index], ""} | T];
hide_passwd([Val | T]) ->
    [Val | hide_passwd(T)];
hide_passwd([]) ->
    [].

%% Called in is_set_ok if logTransferRowStatus is changed
log_tr_table_try_change_status(_, ?'RowStatus_createAndGo',
			       [LogIndex, _LogTrIndex], Cols) ->
    %% The log must exist for which a transfer entry is created
    check_cache(),
    case mnesia:dirty_read({logTable, LogIndex}) of
	[_Found] ->
	    {noError, 0};
	[] ->
	    {noCreation, ?logTransferRowStatus}
    end;
log_tr_table_try_change_status(_, ?'RowStatus_createAndWait',
			       [LogIndex, _LogTrIndex], Cols) ->
    %% The log must exist for which a transfer entry is created
    check_cache(),
    case mnesia:dirty_read({logTable, LogIndex}) of
	[_Found] ->
	    {noError, 0};
	[] ->
	    {noCreation, ?logTransferRowStatus}
    end;
log_tr_table_try_change_status(_, ?'RowStatus_active', RowIndex, Cols) ->
    %% Check that the row isn't active already
    [TR] = mnesia:dirty_read({logTransferTable, lidx(RowIndex)}),
    case TR#logTransferTable.logTransferRowStatus of
	?'RowStatus_active' ->
	    {inconsistentValue, ?logTransferRowStatus};
	_Found ->
	    {noError, 0}
    end;
log_tr_table_try_change_status(_,_, _, _) ->
    {noError, 0}.


%% Called in set if logRowStatus is changed
log_tr_table_change_status(_, ?'RowStatus_createAndGo', RowIndex, _) ->
    case activate_transfer(RowIndex) of
	ok ->
	    {noError, 0};
	error ->
	    mnesia:delete({logTransferTable, lidx(RowIndex)}),
	    {genErr, ?logTransferRowStatus}
    end;
log_tr_table_change_status(_, ?'RowStatus_active', RowIndex, _) ->
    case activate_transfer(RowIndex) of
	ok ->
	    {noError, 0};
	error ->
	    [TR] = mnesia:read(lidx(RowIndex)),
	    mnesia:write(TR#logTransferTable{
				logTransferRowStatus =
				   ?logTransferRowStatus_notInService}),
	    {genErr, ?logTransferRowStatus}
    end;
log_tr_table_change_status(_, ?'RowStatus_notInService', RowIndex, _) ->
    deactivate_transfer(RowIndex),
    {noError, 0};
log_tr_table_change_status(_, ?'RowStatus_destroy', RowIndex, _) ->
    deactivate_transfer(RowIndex),
    {noError, 0};
log_tr_table_change_status(_, _, _, _) ->
    {noError, 0}.

%%-----------------------------------------------------------------
%% "backwards" instrumentation functions  alarm -> trap
%%-----------------------------------------------------------------
log_wrap_too_often(#alarm{sender = LogName}) ->
%    %% BUG IN THIS ONE - query returns all indexes!!!
%    %% log_snmp:log_wrap_too_often({alarm, 1, 1, 1, "board", 1, 1, 1, 1}).
%    Handle = query [Log.logIndex || Log <- table(logTable),
%				    Log.logName = LogName] end,
%    case catch mnesia:ets(fun() -> mnemosyne:eval(Handle) end) of
%	[LogIndex] ->
    case mnesia:dirty_match_object(?wildLogTable#logTable{logName = LogName}) of
	[#logTable{logIndex = LogIndex}] ->
	    {ok, {?logEntry ++ [?logName, LogIndex], [0,0], []}};
	_ ->
	    {ok, {[0,0], [0,0], []}}
    end.

log_file_error(#alarm{sender = LogName, cause = Cause}) ->
    invalidate_cache(), % make next get* operation read new values
    COid = snmp_cause(Cause),
%    Handle = query [Log.logIndex || Log <- table(logTable),
%				    Log.logName = LogName] end,
%    case catch mnesia:ets(fun() -> mnemosyne:eval(Handle) end) of
%	[LogIndex] ->
    case mnesia:dirty_match_object(?wildLogTable#logTable{logName = LogName}) of
	[#logTable{logIndex = LogIndex}] ->
	    {ok, {?logEntry ++ [?logName, LogIndex], COid, []}};
	_ ->
	    {ok, {[0,0], COid, []}}
    end.

snmp_cause(enospc) -> ?logNoSpaceLeft;
snmp_cause(edquot) -> ?logNoSpaceLeft;
snmp_cause(_) -> ?logMediaBroken.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
transfer(#logTransferTable{key = {LogIndex, LogTrIndex},
			   logTransferFTPAddress = Address,
			   logTransferFTPUser    = User,
			   logTransferFTPPasswd  = Passwd,
			   logTransferFTPFile    = DestFile},
	 {M,F,A}) ->
    process_flag(priority, low),
    SearchF = (catch apply(M, F, [search, LogIndex, LogTrIndex | A])),
    FtpAddress = list_to_tuple(Address),
    exit(log:transfer(FtpAddress, User, Passwd, DestFile, SearchF)).

done(Index) ->
    [TR] = mnesia:dirty_read({logTransferTable, Index}),
    mnesia:dirty_write(TR#logTransferTable{logTransferLastResult = 
					   ?logTransferLastResult_ok,
					   logTransferRowStatus =
					   ?logTransferRowStatus_notInService}).
    
error(Index, Err) ->
    [TR] = mnesia:dirty_read({logTransferTable, Index}),
    mnesia:dirty_write(TR#logTransferTable{logTransferLastResult = Err,
					   logTransferRowStatus =
					   ?logTransferRowStatus_notInService}).

lidx([LogIndex, LogTrIndex]) -> {LogIndex, LogTrIndex}.

get_type_func_i(TypeOid, Types) ->
    case lists:keysearch(TypeOid, 2, Types) of
	{value, {_, _, TypeFunc}} -> {ok, TypeFunc};
	_ -> {error, {no_such_type, TypeOid}}
    end.

default_log_search(Name, Cont) ->
    case disk_log:chunk(Name, Cont) of
	eof ->
	    eof;
	{error, R} ->
	    {error, R};
	{NCont, ListOfTerms} ->
	    List = lists:map(fun(Term) ->
				     io_lib:format("~999999p~n", [Term])
			     end, ListOfTerms),
	    {NCont, list_to_binary(List)}
    end.

%%-----------------------------------------------------------------
%% Loop through all logs persistently stored in logTable.  For
%% each log, check if there is a corresponding log created in log.
%% If it is, set admin and operstatus.  If not, check if the log was
%% created by someone else.  If it was, set the logOperStatus to down,
%% otherwise, we have created the log, so we must recreate it.  If
%% there are logs in the log that are not in LogTable, insert
%% them.
%%-----------------------------------------------------------------
check_logs() ->
    check_logs(mnesia:dirty_first(logTable), log:get_logs(), 1).

check_logs('$end_of_table', Logs, Next) ->
    insert_new_logs(Logs, [], Next);
check_logs(Index, Logs, Next) ->
    [Log] = mnesia:dirty_read({logTable, Index}),
    #logTable{logName = Name, logAdminStatus = Admin,
	      logOperStatus = Oper,
	      logOwner = Owner} = Log,
    {NAdmin, NOper} =
	case lists:keysearch(Name, #log.name, Logs) of
	    {value, _} ->
		case set_status(Name, Admin) of
		    up -> {Admin, ?logOperStatus_up};
		    down -> {Admin, ?logOperStatus_down}
		end;
	    _ when Owner /= ?AgentOwner ->
		%% The application that created the log must recreate it
		%% and set the Oper status tp 'up' when it starts.
		{Admin, ?logOperStatus_down};
	    _ ->
		{Log#logTable.logAdminStatus, ?logOperStatus_down}
	end,
    mnesia:dirty_write(Log#logTable{logAdminStatus = NAdmin,
				    logOperStatus = NOper}),
    NLogs = lists:keydelete(Name, #log.name, Logs), 
    check_logs(mnesia:dirty_next(logTable, Index), NLogs, max(Next, Index+1)).

insert_new_logs([#log{name = Name, wrapt = WrapT, admin_status = Admin,
		      type = Type, oper_status = Oper} | Logs], Types, Index) ->
    case disk_log:info(Name) of
	Info when list(Info) ->
	    {value, {_, {MaxB, MaxF}}} = lists:keysearch(size, 1, Info),
	    {value, {_, Cnt}} = lists:keysearch(items, 1, Info),
	    Size = MaxB * MaxF,
	    WrapP = 100 div MaxF,
	    NWrapT = if WrapT == undefined -> ?default_logMinWrapTime;
			WrapT < 3600 -> 1;
			true -> WrapT div 3600  end,
	    L = #logTable{logIndex = Index, logName = Name,
			  logType = get_type_oid(Type, Types),
			  logMaxSize = Size, logWrapPercentage = WrapP,
			  logOwner = ?AgentOwner,
			  logRowStatus = ?'RowStatus_active',
			  logNumberOfRecords = Cnt,
			  logMinWrapTime = NWrapT},
	    L2 = set_status2(L, Admin, Oper),
	    mnesia:dirty_write(L2),
	    insert_new_logs(Logs, Types, Index + 1);
	_ ->
	    insert_new_logs(Logs, Types, Index)
    end;
insert_new_logs([], _Types, _Index) ->
    ok.

max(X, Y) when X >= Y -> X;
max(X, Y) -> Y.

set_status2(Log, up, up) ->
    Log#logTable{logAdminStatus = ?logAdminStatus_up,
		 logOperStatus = ?logOperStatus_up};
set_status2(Log, up, down) ->
    Log#logTable{logAdminStatus = ?logAdminStatus_up,
		 logOperStatus = ?logOperStatus_down};
set_status2(Log, down, down) ->
    Log#logTable{logAdminStatus = ?logAdminStatus_down,
		 logOperStatus = ?logOperStatus_down};
set_status2(Log, down, up) ->
    Log#logTable{logAdminStatus = ?logAdminStatus_down,
		 logOperStatus = ?logOperStatus_up}.


validate_creation(LogType, LogIndex, Cols) ->
    case get_type_func(LogType) of
	{ok, {M,F,A}} ->
	    case catch apply(M, F, [validate_creation, LogIndex, Cols | A]) of
		true -> {noError, 0};
		false -> {inconsistentValue, ?logType};

		{'EXIT', _} -> {genErr, ?logRowStatus};
		Else -> Else
	    end;
	_ ->
	    {wrongValue, ?logType}
    end.
	    
create_log(Log) ->
    #logTable{logName = Name, logType = TypeOid, logAdminStatus = Admin} = Log,
    case get_type_func(TypeOid) of
	{ok, {M,F,A}} ->
	    case catch apply(M, F, [create, Log | A]) of
		ok ->
		    Oper = case set_status(Name, Admin) of
			       up -> ?logOperStatus_up;
			       down -> ?logOperStatus_down
			   end,
		    mnesia:dirty_write(Log#logTable{logOperStatus = Oper}),
		    ok;
		_ ->
		    error
	    end;
	_ ->
	    error
    end.
	    
set_status(Name, ?logAdminStatus_up) ->
    log:set_admin_status(Name, up);
set_status(Name, ?logAdminStatus_down) ->
    log:set_admin_status(Name, down).



update_log_table(Types) ->
    update_log_table(mnesia:dirty_first(logTable), log:get_logs(), Types, 1).

update_log_table('$end_of_table', Logs, Types, Next) ->
    insert_new_logs(Logs, Types, Next);
update_log_table(Index, Logs, Types, Next) ->
    [Log] = mnesia:dirty_read({logTable, Index}),
    #logTable{logName = Name} = Log,
    case lists:keysearch(Name, #log.name, Logs) of
	{value, #log{type = Type, admin_status = Admin, oper_status = Oper}} ->
	    %% Check ths type again - it might not be unknown anymore
	    TypeOid = get_type_oid(Type, Types),
	    Log2 = set_status2(Log#logTable{logType = TypeOid}, Admin, Oper),
	    {value, {_, Cnt}} = lists:keysearch(items, 1, disk_log:info(Name)),
	    mnesia:dirty_write(Log2#logTable{logNumberOfRecords = Cnt});
	_ ->
	    mnesia:dirty_delete({logTable, Index})
    end,
    NLogs = lists:keydelete(Name, #log.name, Logs), 
    update_log_table(mnesia:dirty_next(logTable, Index), NLogs,
		     Types, max(Index+1, Next)).

get_type_oid(Type, Types) ->
    case lists:keysearch(Type, 1, Types) of
	{value, {_, TypeOid, _}} -> TypeOid;
	_ -> [0, 0]
    end.

get_time() ->
    {M,S,U} = erlang:now(),
    1000000000 * M + 1000 * S + (U div 1000).

set_log_status(L, Admin, Oper) ->
    L#logTable{logAdminStatus = Admin, logOperStatus = Oper}.

init_services() ->
    S = case application:get_env(eva, use_snmpea_log) of
	    {ok, true} ->
		spawn(log_snmpea, init, []),
		[log_snmpea];
	    _ ->
		[]
	end,
    S.

stop_services([]) -> ok;
stop_services([log_snmpea|T]) ->
    log_snmpea:stop(),
    stop_services(T).
