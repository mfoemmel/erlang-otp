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
%

-module(odbc).

-behaviour(gen_server).

-include("odbc.hrl"). %% DEPRECATED
-include("odbc_internal.hrl").

%% API --------------------------------------------------------------------

-export([connect/2, disconnect/1, commit/2, sql_query/2, sql_query/3,
	 select_count/2, select_count/3, first/1, first/2, last/1,
	 last/2, next/1, next/2, prev/1, prev/2, select/3, select/4]).

%%%%%%%%%%%%%%%%%%%% DEPRECATED %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Basic API
-export([sqlBindColumn/3, sqlBindColumn/4]).
-export([sqlCloseHandle/1, sqlCloseHandle/2]).
-export([sqlConnect/4, sqlConnect/5]).
-export([sqlDescribeCol/2, sqlDescribeCol/3]).
-export([sqlDisConnect/1, sqlDisConnect/2]).
-export([sqlEndTran/2, sqlEndTran/3]).
-export([sqlExecDirect/2, sqlExecDirect/3]).
-export([sqlFetch/1, sqlFetch/2]).
-export([sqlNumResultCols/1, sqlNumResultCols/2]).
-export([sqlRowCount/1, sqlRowCount/2]).
-export([sqlSetConnectAttr/3, sqlSetConnectAttr/4]).
-export([columnRef/0]).
-export([readData/2, readData/3]).

%% Utility API
-export([erl_connect/2, erl_connect/3, erl_connect/4, erl_connect/5]).
-export([erl_executeStmt/2, erl_executeStmt/3]).
-export([erl_disconnect/1, erl_disconnect/2]).

%-export([select/3, info/1, select_position/3, update_position/2]).

%% Start/stop
-export([start_link/2, start_link/3]).
-export([stop/1, stop/2]).
-export([start_link_sup/2, start_link_sup/3]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%-------------------------------------------------------------------------
%% supervisor callbacks
-export([start_link_sup/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

%% start/stop debugging of erlang control process
-export([debugerl/2, debugerl/3]).
%% start/stop debugging of c-program (port program)
-export([debugc/2]).

%%--------------------------------------------------------------------------
%% Internal state
-record(state, {port,                        % The port to the c-program
		reply_to,		     % gen_server From parameter 
		owner,                       % Pid of the connection owner
		result_set = undefined,      % exists | undefined
		auto_commit_mode = on,       % on | off
		%% Indicates if first, last and "select absolut"
		%% is supported by the odbc driver.
		absolute_pos,                % true | false  
		%% Indicates if prev and "select relative"
		%% is supported by the odbc driver.
		relative_pos,                % true | false
		%% connecting | connected | disconnecting
		state = connecting,	     
		connected = false,           % DEPRECATED
		fetchdata = false,           % DEPRECATED
		columnvector = []            % DEPRECATED
	       }).

%%--------------------------------------------------------------------------

%%%=========================================================================
%%%  API
%%%=========================================================================

%%-------------------------------------------------------------------------
%% connect(ConnectionStr, Options) -> {ok, ConnectionReferense} |
%%                                    {error, Reason}
%% Description: Spawns an erlang control process that will open a port
%%              to a c-process that uses the ODBC API to open a connection
%%              to the database. 
%%-------------------------------------------------------------------------
connect(ConnectionStr, Options) ->
    
    %% Just in case, most of the time this function call
    %% will return {error,{already_started,odbc}}. This is expected.
    application:start(odbc),
    
    %% Spawn the erlang control process.
    case supervisor:start_child(odbc_sup, [[{client, self()}]]) of
	{ok, Pid} ->
	    connect(Pid, ConnectionStr, Options);
	{error, Reason} ->
	    {error, Reason}
    end.

%%--------------------------------------------------------------------------
%% disconnect(ConnectionReferense) -> ok 
%%                                    
%% Description: Disconnects from the database and terminates both the erlang
%%              control process and the database handling c-process. 
%%--------------------------------------------------------------------------
disconnect(ConnectionReference) ->
    ODBCCmd = [?CLOSE_CONNECTION],
    call(ConnectionReference, {disconnect, ODBCCmd}, ?DEFAULT_TIMEOUT).


%%--------------------------------------------------------------------------
%% commit(ConnectionReference, CommitMode, <TimeOut>) ->  
%%                                    
%% Description: Commits or rollbacks a transaction. Needed on connections
%%              where automatic commit is turned off.  
%%--------------------------------------------------------------------------
commit(ConnectionReference, CommitMode) ->
	commit(ConnectionReference, CommitMode, ?DEFAULT_TIMEOUT).

commit(ConnectionReference, CommitMode, TimeOut) ->
	ODBCCmd = [?COMMIT_TRANSACTION, commit_mode(CommitMode)],
	call(ConnectionReference, {commit, ODBCCmd}, TimeOut).

%%--------------------------------------------------------------------------
%% sql_query(ConnectionReference, SQLQuery, <TimeOut>) ->  
%%                                    
%% Description: Executes a SQL query. If it is a SELECT query the
%%              result set is returned, otherwise the number of affected 
%%       	rows are returned.
%%--------------------------------------------------------------------------
sql_query(ConnectionReference, SQLQuery) ->
	sql_query(ConnectionReference, SQLQuery, ?DEFAULT_TIMEOUT).

sql_query(ConnectionReference, SQLQuery, TimeOut) ->
	ODBCCmd = [?QUERY, SQLQuery],
	call(ConnectionReference, {sql_query, ODBCCmd}, TimeOut).

%%--------------------------------------------------------------------------
%% select_count(ConnectionReference, SQLQuery, <TimeOut>) ->  
%%                                    
%% Description: Executes a SQL SELECT query and associates the result set
%%              with the connection. A cursor is positioned before
%%        	the first row in the result set and the number of
%%	        rows in the result set is returned.
%%--------------------------------------------------------------------------
select_count(ConnectionReference, SQLQuery) ->	
    select_count(ConnectionReference, SQLQuery, ?DEFAULT_TIMEOUT).

select_count(ConnectionReference, SQLQuery, TimeOut) ->	
    ODBCCmd = [?SELECT_COUNT, SQLQuery],
    call(ConnectionReference, {select_count, ODBCCmd}, TimeOut).

%%--------------------------------------------------------------------------
%% first(ConnectionReference, <TimeOut>) ->  
%%                                    
%% Description: Selects the first row in the current result set. The cursor
%%            : is positioned at this row. 
%%--------------------------------------------------------------------------
first(ConnectionReference) ->	
    first(ConnectionReference, ?DEFAULT_TIMEOUT).	
    
first(ConnectionReference, TimeOut) ->	
    ODBCCmd = [?SELECT, ?SELECT_FIRST],
    call(ConnectionReference, {select_cmd, absolute, ODBCCmd}, TimeOut).

%%--------------------------------------------------------------------------
%% last(ConnectionReference, <TimeOut>) ->  
%%                                    
%% Description: Selects the last row in the current result set. The cursor
%%            : is positioned at this row. 
%%--------------------------------------------------------------------------
last(ConnectionReference) ->	
    last(ConnectionReference, ?DEFAULT_TIMEOUT).	
   
last(ConnectionReference, TimeOut) ->	
    ODBCCmd = [?SELECT, ?SELECT_LAST],
    call(ConnectionReference, {select_cmd, absolute, ODBCCmd}, TimeOut).

%%--------------------------------------------------------------------------
%% next(ConnectionReference, <TimeOut>) ->  
%%                                    
%% Description: Selects the next row relative the current cursor position 
%%            : in the current result set. The cursor is positioned at 
%%            : this row. 
%%--------------------------------------------------------------------------
next(ConnectionReference) ->	
    next(ConnectionReference, ?DEFAULT_TIMEOUT).	
    
next(ConnectionReference, TimeOut) ->	
    ODBCCmd = [?SELECT, ?SELECT_NEXT],
    call(ConnectionReference, {select_cmd, next, ODBCCmd}, TimeOut).

%%--------------------------------------------------------------------------
%% prev(ConnectionReference, <TimeOut>) ->  
%%                                    
%% Description: Selects the previous row relative the current cursor 
%%            : position in the current result set. The cursor is
%%            : positioned at this row. 
%%--------------------------------------------------------------------------
prev(ConnectionReference) ->	
    prev(ConnectionReference, ?DEFAULT_TIMEOUT).	
    
prev(ConnectionReference, TimeOut) ->	
    ODBCCmd = [?SELECT, ?SELECT_PREV],
    call(ConnectionReference, {select_cmd, relative, ODBCCmd}, TimeOut).

%%--------------------------------------------------------------------------
%% select(ConnectionReference, <Timeout>) ->  
%%                                   
%% Description: Selects <N> rows. If <Position> is next it is
%%              semanticly eqvivivalent of calling next/[1,2] <N>
%%              times. If <Position> is {relative, Pos} <Pos> will be
%%              used as an offset from the current cursor position to
%%              determine the first selected row. If <Position> is
%%              {absolute, Pos}, <Pos> will be the number of the first
%%              row selected. After this function has returned the
%%              cursor is positioned at the last selected row.
%%--------------------------------------------------------------------------
select(ConnectionReference, Position, N) ->
    select(ConnectionReference, Position, N, ?DEFAULT_TIMEOUT).

select(ConnectionReference, Position, N, TimeOut) ->

    {CursorRelation, OffSet} = 
	case Position of
	    next ->
	       {next, ?DUMMY_OFFSET};
	    _ ->
		Position
	end,

    ODBCCmd = [?SELECT, cursor_relation(CursorRelation),
	       integer_to_list(OffSet), ";", integer_to_list(N), ";"],
    call(ConnectionReference, {select_cmd, CursorRelation, ODBCCmd},
	 TimeOut).

%%%=========================================================================
%%% Start/stop
%%%=========================================================================
%%--------------------------------------------------------------------------
%% start_link_sup(Args) ->  
%%                                    
%% Description: Callback function for the odbc supervisor. It is called 
%%            : when connect/2 calls supervisor:start_child/2 to start an 
%%            : instance of the erlang odbc control process.
%%--------------------------------------------------------------------------
start_link_sup(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%% Stop functionality is handled by disconnect/1

%%%========================================================================
%%% Callback functions from gen_server
%%%========================================================================

%%-------------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          {stop, Reason}
%%-------------------------------------------------------------------------
init(Args) ->
    process_flag(trap_exit, true),
    {value, {client, ClientPid}} = lists:keysearch(client, 1, Args),
    
    erlang:monitor(process, ClientPid),
    
    %% Start the port program (a c program) that utilizes the odbc driver 
    case os:find_executable(?SERVERPROG, ?SERVERDIR) of
	FileName when list(FileName)->
	    Port  = open_port({spawn, FileName},
			      [{packet, ?LENGTH_INDICATOR_SIZE}, binary]),
	    State = #state{port = Port, owner = ClientPid},
	    {ok, State};
	false ->
	    {stop, "Can't find the port-program: odbcserver executable."}
    end.
		    
%%--------------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, Reply, State}     (terminate/2 is called)
%%--------------------------------------------------------------------------
handle_call({Client, Msg}, From, State = #state{owner = Client}) ->
    handle_msg(Msg, State#state{reply_to = From});

handle_call(_, _, State) ->
    {reply, {error, process_not_owner_of_odbc_connection}, State}.

%%--------------------------------------------------------------------------
%% Func: handle_msg/3 - (help function to handle_call/3)
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, Reply, State}     (terminate/2 is called)
%% Description: 
%%--------------------------------------------------------------------------
handle_msg({connect, ODBCCmd, AutoCommitMode}, State) ->
    port_command(State#state.port, [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, State#state{auto_commit_mode = AutoCommitMode}};

handle_msg({disconnect, ODBCCmd}, State) ->
    port_command(State#state.port,  [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, State#state{state = disconnecting}, 5000};

handle_msg({commit, _ODBCCmd}, State = #state{auto_commit_mode = on}) ->
    {reply, {error, not_an_explicit_commit_connection}, State};

handle_msg({commit, ODBCCmd}, State = #state{auto_commit_mode = off}) ->
    port_command(State#state.port,  [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, State};

handle_msg({sql_query, ODBCCmd}, State) ->
    port_command(State#state.port,  [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, State#state{result_set = undefined}};

handle_msg({select_count, ODBCCmd}, State) ->
    port_command(State#state.port,  [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, State#state{result_set = exists}};

handle_msg({select_cmd, absolute, ODBCCmd}, 
	   State = #state{result_set = exists, absolute_pos = true}) ->
    port_command(State#state.port,  [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, State};

handle_msg({select_cmd, relative, ODBCCmd}, 
	   State = #state{result_set = exists, relative_pos = true}) ->
    port_command(State#state.port,  [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, State};

handle_msg({select_cmd, next, ODBCCmd},
	   State = #state{result_set = exists}) ->
    port_command(State#state.port,  [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, State};

handle_msg({select_cmd, _Type, _ODBCCmd},
	   State = #state{result_set = undefined}) ->
    {reply, {error, result_set_does_not_exist}, State};

handle_msg({select_cmd, _Type, _ODBCCmd}, State) ->
    {reply, {error, driver_does_not_support_function}, State};

%%%%%%%%%%%%%%%% DEPRECATED CLAUSES OF HANDLE_MSG START %%%%%%%%%%%%%%%%%%%%
%-------------------------DEPRECATED---------------------------------------
%% Open connection to database
handle_msg({control_cmd, {open_connection, ODBCCmd}}, 
	    State = #state{connected = false}) ->
    Port  = State#state.port,
    port_command(Port,  [ODBCCmd, ?STR_TERMINATOR]),
    NewState = State#state{connected = true, 
			   %% Not entirely true but neede so this old
			   %% interface will still work as before
			   state = connected},
    {noreply, NewState};

%-------------------------DEPRECATED----------------------------------------
%% If you try opening a connection that is already open
handle_msg({control_cmd, {open_connection, _ODBCCmd}}, 
	    State = #state{connected = true}) ->
    {reply, {error, "Connection already open", ?SQL_ERROR}, State};
%-------------------------DEPRECATED----------------------------------------
%% Close connection to database
handle_msg({control_cmd, {close_connection, ODBCCmd}}, 
	    State = #state{connected = true}) ->
    Port = State#state.port,
    NewState = State#state{connected = false},
    port_command(Port,  [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, NewState};
%-------------------------DEPRECATED----------------------------------------
%% If you try closing a connection that is already closed
handle_msg({control_cmd, {close_connection, _ODBCCmd}}, State = 
	    #state{connected = false}) ->
    {reply, {error, "Connection already closed", ?SQL_ERROR}, State};
%-------------------------DEPRECATED----------------------------------------
%---------------------------------------------------------------------------
%% ODBC commands - require that we have a connection to the database
handle_msg({db_cmd, _Cmd}, 
            #state{connected = false} = State) ->
    {reply, {error, "Not connected", ?SQL_ERROR}, State};
%-------------------------DEPRECATED----------------------------------------
handle_msg({db_cmd, {bind_column, ODBCCmd, ColNum, MemRef}},
	   State = #state{connected = true}) ->
    Port = State#state.port,
    ColumnVec = State#state.columnvector,
    NewColumnVec = insert({MemRef,ColNum}, ColumnVec),
    NewState = State#state{columnvector = NewColumnVec},
    NewODBCCmd = [ODBCCmd, integer_to_list(ColNum)],
    port_command(Port,  [NewODBCCmd, ?STR_TERMINATOR]),
    {noreply, NewState};
%-------------------------DEPRECATED----------------------------------------
handle_msg({db_cmd,{close_handle, ODBCCmd}}, State =
	    #state{connected = true}) ->

    Port = State#state.port,
    ColumnVect    = State#state.columnvector,
    NewColumnVect = delete(ColumnVect),
    NewState = State#state{columnvector = NewColumnVect, fetchdata = false},
    port_command(Port,  [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, NewState};
%-------------------------DEPRECATED----------------------------------------
handle_msg({db_cmd, {read_buffer, ODBCCmd, ColumnRef}}, 
	    State = #state{connected = true, fetchdata = true}) ->
    Port       = State#state.port,
    ColumnVect = State#state.columnvector,
    CNumber    = lookup(ColumnRef,ColumnVect),

    case CNumber of
	not_found ->
	    {reply, {error, column_not_found, ?SQL_ERROR}, State};
	_ ->
	    NewODBCCmd = [ODBCCmd, integer_to_list(CNumber)],
	    port_command(Port,  [NewODBCCmd, ?STR_TERMINATOR]),
	    {noreply, State}
    end;
%-------------------------DEPRECATED----------------------------------------
handle_msg({db_cmd, {read_buffer, _ODBCCmd, _ColumnRef}}, 
	    #state{fetchdata = false} = State) ->
    {reply, {error, no_data_fetched, ?SQL_ERROR}, State};
%-------------------------DEPRECATED----------------------------------------
handle_msg({db_cmd, {fetch, ODBCCmd}}, State = 
	    #state{connected = true}) ->
    Port = State#state.port,
    port_command(Port,  [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, State#state{fetchdata = true}};
%-------------------------DEPRECATED----------------------------------------
%% Tag = execute | execdir | describeColumn | endTran | 
%% | numResultCols | rowCount | setConnectAttr 
handle_msg({db_cmd, {_Tag, ODBCCmd}}, State = 
	   #state{connected = true}) ->
    Port = State#state.port,
    port_command(Port,  [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, State};

%%%%%%%%%%%%%%%% DEPRECATED CLAUSES OF HANDLE_MSG END %%%%%%%%%%%%%%%%%%%%%%

%---------------------------------------------------------------------------
%% Catch all - throws away unknown messages.
handle_msg(Request, State) ->
    error_logger:error_msg("ODBC: received unexpected request: ~p~n", 
			   [Request]),
    {reply, {error, {did_not_understand_request, Request}}, State}.

%%--------------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%-------------------------------------------------------------------------
%% Stop Call Back
handle_cast(stop, State) ->
    {stop, normal, State};

%% Send debug request to the port-program.
handle_cast({debugc, DebugCmd}, State) ->
    port_command(State#state.port, [DebugCmd, ?STR_TERMINATOR]),
    {noreply, State};

%% Catch all - throws away unknown messages.
handle_cast(Msg, State) ->
    error_logger:error_msg("ODBC: received unexpected message: ~p~n", 
			   [Msg]),
    {noreply, State}.

%%--------------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%--------------------------------------------------------------------------
handle_info({Port, {data, BinData}}, State = #state{state = connecting, 
						    reply_to = From,
						    port = Port}) ->
    case binary_to_term(BinData) of
	{ok, AbsolutSupport, RelativeSupport} ->
	    NewState = State#state{absolute_pos = AbsolutSupport,
				   relative_pos = RelativeSupport},
	    gen_server:reply(From, ok), 
	    {noreply, NewState#state{state = connected,
				     reply_to = undefined}};
	Error ->
	    gen_server:reply(From, Error), 
	    {stop, {stopped, Error}, State#state{reply_to = undefined}}
    end;
	
handle_info({Port, {data, BinData}}, State = #state{state = connected,
						    reply_to = From,
						    port = Port}) ->
    %% Send the reply from the database (received by the erlang control 
    %% process from the port program) to the waiting client.
    gen_server:reply(From, BinData),
    {noreply, State#state{reply_to = undefined}};

handle_info({Port, {data, BinData}},  State = #state{state = disconnecting,
						     reply_to = From,
						     port = Port}) ->
    gen_server:reply(From, BinData),
    {stop, normal, State};

%---------------------------------------------------------------------------
handle_info(timeout, State = #state{state = disconnecting, 
				    reply_to = From}) ->
    gen_server:reply(From, ok), 
    {stop, {timeout, "Port program is not responding to disconnect, " 
	    "will be killed"}, State};

handle_info({'EXIT', Port, Reason}, State = #state{port = Port}) ->
    case State#state.reply_to of
	undefined ->
	    ok;
	From ->
	    gen_server:reply(From, {error, {port_exit, Reason}})
    end,
    error_logger:error_msg("ODBC: exit signal from port program:~p~n", 
			   [Reason]),
    {stop, {port_exit, Reason}, State};

%% If the owning process dies there is no reson to go on
handle_info({'DOWN', _Ref, _Type, Process, Reason}, State) ->
    {stop, {stopped, {'EXIT', Process, Reason}}, State};
    
%---------------------------------------------------------------------------
% Catch all - throws away unknown messages
handle_info(Info, State) ->
    error_logger:error_msg("ODBC: received unexpected info: ~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------------
%% terminate/2 and code_change/3
%%--------------------------------------------------------------------------

terminate({port_exit, _Reason}, _State) ->
    ok;

terminate(_Reason, State) ->
    Port = State#state.port,
    port_close(Port),
    receive
	{Port, closed} ->
	    ok;
	{'EXIT', Port, normal} ->
	    ok
    after 10000 ->
	    error_logger:error_report("Erlang ODBC-process did not receive "
				      "closed port message.")
    end.

%---------------------------------------------------------------------------
code_change(_Vsn, State, _Extra) ->
    {ok, State}.

%%%========================================================================
%%% Internal functions
%%%========================================================================

connect(ConnectionReferense, ConnectionStr, Options) ->
    {C_AutoCommitMode, ERL_AutoCommitMode} = 
	connection_config(auto_commit, Options),
    TimeOut = connection_config(timeout, Options),
    {C_TraceDriver, _} = connection_config(trace_driver, Options),

    ODBCCmd = 
	[?OPEN_CONNECTION, C_AutoCommitMode, C_TraceDriver, ConnectionStr],
    
    %% Send request, to open a database connection, to the control process.
    case call(ConnectionReferense, 
	      {connect, ODBCCmd, ERL_AutoCommitMode}, TimeOut) of
	ok ->
	    {ok, ConnectionReferense};
	Error ->
	    %% As the connection failed we do not need any process to 
	    %% control it. 
	    cast(ConnectionReferense, stop),
	    Error
    end.

%%--------------------------------------------------------------------------
connection_config(Key, Options) ->
    case lists:keysearch(Key, 1, Options) of
	{value,{Key, on}} ->
	    {?ON, on};
	{value,{Key, off}} ->
	    {?OFF, off};
	{value,{Key, Value}} ->
	    Value;
	_ ->
	    connection_default(Key)
    end.

%%--------------------------------------------------------------------------
connection_default(auto_commit) ->
    {?ON, on};

connection_default(timeout) ->
    ?DEFAULT_TIMEOUT;

connection_default(trace_driver) ->
    {?OFF, off}.
%%--------------------------------------------------------------------------
commit_mode(commit) ->
    ?COMMIT;
commit_mode(rollback) ->
    ?ROLLBACK.
%%--------------------------------------------------------------------------
cursor_relation(relative) ->
    ?SELECT_RELATIVE;
cursor_relation(absolute) ->
    ?SELECT_ABSOLUTE;
cursor_relation(next) ->
    ?SELECT_N_NEXT.
%%-------------------------------------------------------------------------
call(ConnectionReference, Msg, Timeout) ->
    Result = gen_server:call(ConnectionReference, {self(), Msg}, Timeout),
    if 
	%% Normal case, the result from the port-program is directly 
	%% forwarded to the client
	binary(Result) -> 
	    binary_to_term(Result); 
	true ->  % Special case or error
	    Result
    end.
%%-------------------------------------------------------------------------
cast(ConnectionReference, Msg) ->
    gen_server:cast(ConnectionReference, Msg).

%%%========================================================================
%%% Debug functions
%%%========================================================================

%%--------------------------------------------------------------------------
%% debugerl(Server, OnOff, <Level>) -> ok
%%	Process  - pid() | Name | {global, Name} | {Name, Node} 
%%	OnOff   - on | off
%%      Level   - exported | all
%% Description: Turns on tracing of messages sent and recived by
%%              the server <Process> and tracing on all, or all exported 
%%              functions, according to level <Level>, in this module.
%%              Result will be printed on stdout.
%% Note: This function is only intended for debugging and may not be used
%%       in products. Turning on this tracing will cause the program
%%       to loose performance !!!!!!!!!!!!   
%%--------------------------------------------------------------------------
debugerl(Process, OnOff) ->
    debugerl(Process, OnOff, exported).

debugerl(Process, on, exported) ->
    dbg:tracer(),
    dbg:tp(?MODULE, [{'_', [], [{return_trace}]}]),
    dbg:p(Process, [call, m]),
    ok; 

debugerl(Process, on, all) ->
    dbg:tracer(),
    dbg:tpl(?MODULE, [{'_', [], [{return_trace}]}]),
    dbg:p(Process, [call, m]),
    ok;

debugerl(_Process, off, _Level) ->
    dbg:stop(),
    ok.

%%--------------------------------------------------------------------------
%% debugc(Process, OnOff) -> _
%%	Process  - pid() | Name | {global, Name} | {Name, Node} 
%%	OnOff   - on | off	
%% Description: Turns on/off the c-programs (port programs) debug-printouts.
%% Note: This function is only intended for debugging and may not be used
%%       in products. Turning on these printouts will cause the program
%%       to loose performance !!!!!!!!!!!!
%%--------------------------------------------------------------------------
debugc(Process, on) ->
    CCmd = [?DEBUG, ?ON, ?STR_TERMINATOR],
    gen_server:cast(Process, {debugc, CCmd});
    
debugc(Process, off) ->
    CCmd = [?DEBUG, ?OFF, ?STR_TERMINATOR],
    gen_server:cast(Process, {debugc, CCmd}).

    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                   DEPRECATED INTERFACE                            %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%======================================================================
%%% Basic API
%%%======================================================================

%%------------------------------DEPRECATED----------------------------------
%% sqlBindColumn(Server, ColNum, Ref, <Timeout>) -> Result |
%%						   {error, ErrMsg, ErrCode} 
%%	Server  = pid() | Name | {global, Name} | {Name, Node} 
%%      ColNum  = integer()
%%      Ref     = term()
%%      Timeout = integer() | infinity
%%      Result  = ?SQL_SUCCESS | ?SQL_SUCCESS_WITH_INFO 
%%      ErrMsg   = string()
%%      ErrCode  = ?SQL_INVALID_HANDLE | ?SQL_ERROR
%% Description: Assigns a referens to a column (binds a referens to a  column).
%%--------------------------------------------------------------------------
sqlBindColumn(Server, ColNum, Ref) ->
    sqlBindColumn(Server, ColNum, Ref, ?DEPRECATED_DEFAULT_TIMEOUT).

sqlBindColumn(Server, ColNum, Ref, Timeout)
  when integer(ColNum), reference(Ref) ->
 
    ODBCCmd = ?BIND_COLUMN,   
    deprecated_call(Server,{bind_column, ODBCCmd, ColNum, Ref}, Timeout);

sqlBindColumn(_Server, ColNum, Ref, _Timeout) when integer(ColNum) ->
    exit({badarg, sqlBindColumn, {"Arg 3 is not a reference", Ref}});

sqlBindColumn(_Server, ColNum, _Ref, _Timeout) ->
    exit({badarg, sqlBindColumn, {"Arg 2 is not an integer", ColNum}}).

%%-------------------------DEPRECATED---------------------------------------
%% sqlCloseHandle(Server, <Timeout>) -> Result | {error, ErrMsg, ErrCode} 
%%	Server  = pid() | Name | {global, Name} | {Name, Node} 
%%      Timeout = integer() | infinity
%%      Result  = ?SQL_SUCCESS | ?SQL_SUCCESS_WITH_INFO
%%      ErrMsg   = string()
%%      ErrCode  = ?SQL_INVALID_HANDLE | ?SQL_ERROR 
%% Description: Close Stament handle. CloseHandle is needed when basic SQL 
%%              functions are used.
%%--------------------------------------------------------------------------
sqlCloseHandle(Server) ->
    sqlCloseHandle(Server, ?DEPRECATED_DEFAULT_TIMEOUT).

sqlCloseHandle(Server, Timeout) ->
    ODBCCmd = ?CLOSE_HANDLE, 
    deprecated_call(Server, {close_handle, ODBCCmd}, Timeout).

%%------------------------DEPRECATED--------------------------------------
%% sqlConnect(Server, DSN, UID, PWD, <Timeout>) -> Result | 
%%						   {error, ErrMsg, ErrCode} 
%%	Server  = pid() | Name | {global, Name} | {Name, Node} 
%%      DSN = string() - The name of the database.
%%      UID = string() - The user ID.
%%      PWD = string() - The user's password for the database.
%%      Timeout = integer() | infinity
%%      Result  = ?SQL_SUCCESS | ?SQL_SUCCESS_WITH_INFO 
%%      ErrMsg   = string()
%%      ErrCode  = ?SQL_INVALID_HANDLE | ?SQL_ERROR
%% Description: Establishes a connection to a driver and a database.
%%--------------------------------------------------------------------------
sqlConnect(Server, DSN, UID, PWD) ->
    sqlConnect(Server, DSN, UID, PWD, ?DEPRECATED_DEFAULT_TIMEOUT).

sqlConnect(Server, DSN, UID, PWD, Timeout) 
  when list(DSN), list(UID), list(PWD) ->
    ODBCCmd =  [?OPEN_DB, "DSN=",DSN,";UID=",UID,";PWD=",PWD],
    deprecated_call(Server, {open_connection, ODBCCmd}, Timeout);

sqlConnect(_Server, DSN, UID, PWD, _Timeout) 
   when list(DSN), list(UID) ->
     exit({badarg, sqlConnect, {"Arg 4 is not a string", PWD}});    
 sqlConnect(_Server, DSN, UID, _PWD, _Timeout) 
   when list(DSN) ->
     exit({badarg, sqlConnect, {"Arg 3 is not a string", UID}});
 sqlConnect(_Server, DSN, _UID, _PWD, _Timeout) ->
     exit({badarg, sqlConnect, {"Arg 2 is not a string", DSN}}).
%%----------------------------DEPRECATED-----------------------------------
%% sqlDescribeCol(Server, ColNum, <Timeout>) -> {Result, ColName, Nullable}|
%%				                {error, ErrMsg, ErrCode} 
%%	Server   = pid() | Name | {global, Name} | {Name, Node} 
%%      Timeout  = integer() | infinity
%%      Result   = ?SQL_SUCCESS | ?SQL_SUCCESS_WITH_INFO 
%%      ColName  = string() - The column name.
%%      Nullable = ?SQL_NO_NULLS | ?SQL_NULLABLE | ?SQL_NULLABLE_UNKNOWN
%%      ErrMsg   = string()
%%      ErrCode  = ?SQL_INVALID_HANDLE | ?SQL_ERROR
%% Description:  Returns the result descriptor -- column name, type, column 
%%		 size, decimal digits, and nullability -- for one column 
%%		 in the result set.
%%--------------------------------------------------------------------------
sqlDescribeCol(Server, ColNum) ->
    sqlDescribeCol(Server, ColNum, ?DEPRECATED_DEFAULT_TIMEOUT).

sqlDescribeCol(Server, ColNum, Timeout) when integer(ColNum) ->
    ODBCCmd = [?DESCRIBE_COLUMN, integer_to_list(ColNum)],
    deprecated_call(Server, {describeColumn, ODBCCmd}, Timeout);

sqlDescribeCol(_Server, ColNum, _Timeout) ->
    exit({badarg, sqlDescribeCol, {"Arg 2 is not an integer", ColNum}}).    

%%--------------------------DEPRECATED-------------------------------------
%% sqlDisconnect(Server, <Timeout>) -> Result | {error, ErrMsg, ErrCode} 
%%	Server  = pid() | Name | {global, Name} | {Name, Node} 
%%      Timeout = integer() | infinity
%%      Result  = ?SQL_SUCCESS | ?SQL_SUCCESS_WITH_INFO
%%      ErrMsg   = string()
%%      ErrCode  = ?SQL_INVALID_HANDLE | ?SQL_ERROR 
%% Description: Closes a database connection.
%%--------------------------------------------------------------------------
sqlDisConnect(Server) ->
    sqlDisConnect(Server, ?DEPRECATED_DEFAULT_TIMEOUT).

sqlDisConnect(Server, Timeout) ->
    CloseStr = ?CLOSE_DB,
    deprecated_call(Server,{close_connection, CloseStr}, Timeout).

%%--------------------------DEPRECATED--------------------------------------
%% sqlEndTran(Server, ComplType, <Timeout>) -> Result | 
%%					       {error, ErrMsg, ErrCode} 
%%	Server    = pid() | Name | {global, Name} | {Name, Node} 
%%      ComplType = ?SQL_COMMIT | ?SQL_ROLLBACK
%%      Timeout   = integer() | infinity
%%      Result    = ?SQL_SUCCESS | ?SQL_SUCCESS_WITH_INFO
%%      ErrMsg    = string()
%%      ErrCode   = ?SQL_INVALID_HANDLE | ?SQL_ERROR 
%% Description: Requests a commit or rollback operation for all active 
%%		operations on all statement handles associated with a 
%%		connection. It can only request a commit or rollback 
%%	        operation for a single connection.
%%--------------------------------------------------------------------------
sqlEndTran(Server, ComplType) ->
    sqlEndTran(Server, ComplType, ?DEPRECATED_DEFAULT_TIMEOUT).

sqlEndTran(Server, ComplType, Timeout) ->
    ODBCCmd = [?END_TRANSACTION, ComplType],
    deprecated_call(Server,{endTran, ODBCCmd}, Timeout).

%%-------------------------DEPRECATED--------------------------------------
%% sqlExecDirect(Server, Stmt, <Timeout>) -> Result | 
%%                                          {error, ErrMsg, ErrCode} 
%%	Server  = pid() | Name | {global, Name} | {Name, Node} 
%%      Timeout = integer() | infinity
%%      Result  = ?SQL_SUCCESS | ?SQL_SUCCESS_WITH_INFO
%%      ErrMsg   = string()
%%      ErrCode  = ?SQL_INVALID_HANDLE | ?SQL_ERROR 
%% Description: 
%%--------------------------------------------------------------------------
sqlExecDirect(Server, Stmt) ->
    sqlExecDirect(Server, Stmt, ?DEPRECATED_DEFAULT_TIMEOUT).

sqlExecDirect(Server, Stmt, Timeout) when list(Stmt) ->
    CmdStmt = [?EXEDIR, Stmt],
    deprecated_call(Server, {execdir,CmdStmt}, Timeout);

sqlExecDirect(_Server, Stmt, _Timeout) ->
    exit({badarg, sqlExecDirect, {"Arg 2 is not a string", Stmt}}).    

%%--------------------------DEPRECATED--------------------------------------
%% sqlFetch(Server, <Timeout>) -> Result | {error, ErrMsg, ErrCode} 
%%	Server  = pid() | Name | {global, Name} | {Name, Node} 
%%      Timeout = integer() | infinity
%%      Result  = ?SQL_SUCCESS | ?SQL_SUCCESS_WITH_INFO
%%      ErrMsg   = string()
%%      ErrCode  = ?SQL_INVALID_HANDLE | ?SQL_ERROR 
%% Description: Fetches a row of data from a result set. The driver returns 
%%              data for all columns that were bound to a referens with 
%%              sqlBindCol/[3, 4].
%%--------------------------------------------------------------------------
sqlFetch(Server) ->
    sqlFetch(Server, ?DEPRECATED_DEFAULT_TIMEOUT).

sqlFetch(Server, Timeout) ->
    ODBCCmd = ?FETCH_DATA, 
    deprecated_call(Server,{fetch, ODBCCmd}, Timeout).

%%--------------------DEPRECATED--------------------------------------------
%% sqlNumResultCols(Server, <Timeout>) -> {Result, ColCount}  |
%%                                        {error, ErrMsg, ErrCode} 
%%	Server  = pid() | Name | {global, Name} | {Name, Node} 
%%      Timeout = integer() | infinity
%%      Result  = ?SQL_SUCCESS | ?SQL_SUCCESS_WITH_INFO
%%      ColCount = integer()
%%      ErrMsg   = string()
%%      ErrCode  = ?SQL_INVALID_HANDLE | ?SQL_ERROR 
%% Description: Returns the number of columns in a result set.
%%--------------------------------------------------------------------------
sqlNumResultCols(Server) ->
    sqlNumResultCols(Server, ?DEPRECATED_DEFAULT_TIMEOUT).

sqlNumResultCols(Server, Timeout) ->
    ODBCCmd = ?NUMBER_RESULT_COLUMNS,
    deprecated_call(Server,{numResultCols, ODBCCmd}, Timeout).

%%------------------------DEPRECATED----------------------------------------
%% sqlRowCount(Server, Timeout) -> {Result, RowCount} |
%%                                 {error, ErrMsg, ErrCode} 
%%	Server   = pid() | Name | {global, Name} | {Name, Node} 
%%      Timeout  = integer() | infinity
%%      Result   = ?SQL_SUCCESS | ?SQL_SUCCESS_WITH_INFO
%%      RowCount = integer()
%%      ErrMsg   = string()
%%      ErrCode  = ?SQL_INVALID_HANDLE | ?SQL_ERROR 
%% Description:  Returns the number of rows affected by an UPDATE, INSERT, 
%%               or DELETE statement.
%%--------------------------------------------------------------------------
sqlRowCount(Server) ->
    sqlRowCount(Server, ?DEPRECATED_DEFAULT_TIMEOUT).

sqlRowCount(Server, Timeout) ->
    ODBCCmd = ?ROW_COUNT,
    deprecated_call(Server,{rowCount, ODBCCmd},Timeout).
    
%%--------------------------DEPRECATED-------------------------------------
%% sqlSetConnectAttr(Server, Attr, Value, <Timeout>) ->  Result |
%%					          {error, ErrMsg, ErrCode} 
%%	Server  = pid() | Name | {global, Name} | {Name, Node} 
%%      Timeout = integer() | infinity
%%      Attr = integer() - supported are: ?SQL_ATTR_AUTOCOMMIT,
%%                                        ?SQL_ATTR_TRACE,
%%					  ?SQL_ATTR_TRACEFILE
%%      Value = string() | integer()
%%      Result  = ?SQL_SUCCESS | ?SQL_SUCCESS_WITH_INFO
%%      ErrMsg   = string()
%%      ErrCode  = ?SQL_INVALID_HANDLE | ?SQL_ERROR 
%% Description: Sets attributes that govern aspects of connections.
%%              Driver-specific attributes are not supported through macros,
%%		but can be set, if they are strings or signed/unsigned 
%%              long integers.
%%--------------------------------------------------------------------------
sqlSetConnectAttr(Server, Attr, Value) ->
    sqlSetConnectAttr(Server, Attr, Value, ?DEPRECATED_DEFAULT_TIMEOUT).

sqlSetConnectAttr(Server, Attr, Value, Timeout) ->
    Cmd = if 
	      number(Value) ->
		  [?INT_VALUE, integer_to_list(Attr),
		   ";", integer_to_list(Value), ";"];
	      list(Value) ->
		  [?STR_VALUE, integer_to_list(Attr), ";", Value, ";"];
	      true ->
		  exit({badarg, sqlSetConnectAttr, {"Arg 3 is not a string "
			"or integer", Value}})
	  end,
    ODBCCmd = [?SET_ATTRIBUTE, Cmd],
    
    deprecated_call(Server, {setConnectAttr, ODBCCmd}, Timeout).

%%---------------------------DEPRECATED-----------------------------------
%% readData(Server, Ref, <Timeout>) ->  {ok, value} | 
%%                                      {error, ErrMsg, ErrCode}
%%	Server  = pid() | Name | {global, Name} | {Name, Node} 
%%      Ref     = ref()
%%      Timeout = integer() | infinity
%%      Result  = ?SQL_SUCCESS | ?SQL_SUCCESS_WITH_INFO
%%      ErrMsg  = string()
%%      ErrCode = ?SQL_INVALID_HANDLE | ?SQL_ERROR 
%% Description: Returns data from the sqlFetch/[1,2] call.
%%--------------------------------------------------------------------------
readData(Server, Ref) ->
    readData(Server, Ref, ?DEPRECATED_DEFAULT_TIMEOUT).

readData(Server, Ref, Timeout) ->
    ODBCCmd = ?READ_BUFFER,
    deprecated_call(Server, {read_buffer, ODBCCmd, Ref}, Timeout).

%%-------------------DEPRECATED------------------------------------
%% columnRef() -> ref()
%% Description: Generate a referens for a column. 
%%--------------------------------------------------------------------------
columnRef() ->
    Ref = erlang:make_ref(),
    {ok, Ref}.

%%%======================================================================
%%% Utility API
%%%======================================================================

%%--------------------------DEPRECATED----------------------------------
%% erl_connect(Server, ConnectStr, <Timeout>) ->  ok, | 
%%                                                {error, ErrMsg, ErrCode} 
%%	Server     = pid() | Name | {global, Name} | {Name, Node} 
%%      ConnectStr = ConnectStr = string() ex:
%%		     "DSN=Oracle8;DBQ=gandalf;UID=test;PWD=foobar" see
%%                    SQLDriverConnect in Microsoft ODBC 3.0, Programmer's 
%%                    Reference and SDK Guide 
%%      Timeout    = integer() | infinity
%%      ErrMsg     = string()
%%      ErrCode    = ?SQL_INVALID_HANDLE | ?SQL_ERROR
%% Description:Opens a connection to a database. There can be only one open 
%%             database connection per server.
%%--------------------------------------------------------------------------
erl_connect(Server, ConnectStr) ->
    erl_connect(Server, ConnectStr, ?DEPRECATED_DEFAULT_TIMEOUT).

erl_connect(Server, ConnectStr, Timeout) when list(ConnectStr) ->
    ODBCCmd = [?OPEN_DB, ConnectStr],
    Result = deprecated_call(Server,{open_connection, ODBCCmd}, Timeout),

    case Result of
	{error, ErrMsg, ErrCode} ->
	    {error,ErrMsg, ErrCode};
	_Other -> %% ?SQL_SUCCESS | ?SQL_SUCCESS_WITH_INFO
	    ok
    end;

erl_connect(_Server, ConnectStr, _Timeout) ->
    exit({badarg, erl_connect, {"Arg 2 is not a string", ConnectStr}}).    

%%-----------------------DEPRECATED-----------------------------------------
%% erl_connect(Server, DSN, UID, PWD, <Timeout>) ->  ok, | 
%%                                                 {error, ErrMsg, ErrCode} 
%%	Server  = pid() | Name | {global, Name} | {Name, Node} 
%%      DNS     = string() - The name of the database.
%%      UID     = string() - The user ID.
%%      PWD     = string() - The user's password for the database.
%%      Timeout = integer() | infinity
%%      ErrMsg     = string()
%%      ErrCode    = ?SQL_INVALID_HANDLE | ?SQL_ERROR
%% Description:Opens a connection to a database. There can be only one open 
%%             sdatabase connection per server.
%%-------------------------------------------------------------------------
erl_connect(Server, DSN, UID, PWD) ->
    erl_connect(Server, DSN, UID, PWD, ?DEPRECATED_DEFAULT_TIMEOUT).

erl_connect(Server, DSN, UID, PWD, Timeout) 
  when list(DSN), list(UID), list(PWD) ->
    ConnectStr = ["DSN=",DSN,";UID=",UID,";PWD=",PWD],
    erl_connect(Server, ConnectStr, Timeout);

erl_connect(_Server, DSN, UID, PWD, _Timeout) 
  when list(DSN), list(UID) ->
    exit({badarg, erl_connect, {"Arg 4 is not a string", PWD}});

erl_connect(_Server, DSN, UID, _PWD, _Timeout) 
  when list(DSN) ->
    exit({badarg, erl_connect, {"Arg 3 is not a string", UID}});

erl_connect(_Server, DSN, _UID, _PWD, _Timeout) ->
         exit({badarg, erl_connect, {"Arg 2 is not a string", DSN}}).

%%--------------------------DEPRECATED--------------------------------------
%% erl_executeStmt(Server, Stmt, <Timeout>) -> {updated, NRows} | 
%%                                            {selected, [ColName], [Row]} |
%%                                            {error, ErrMsg}
%%	Server  = pid() | Name | {global, Name} | {Name, Node} 
%%      Stmt    = string() - SQL query
%%      Timeout = integer() | infinity
%%      NRows = integer()
%%      ColName = string()
%%      Row = [Value] - Values from the row of the resulting table.
%%      Value = string() | null
%% Description: Executes a single SQL statement. All changes to the data 
%%              source are automatically committed if successful.
%%-------------------------------------------------------------------------
erl_executeStmt(Server, Stmt) ->
    erl_executeStmt(Server, Stmt, ?DEPRECATED_DEFAULT_TIMEOUT).

erl_executeStmt(Server, Stmt, Timeout) when list(Stmt) ->
    SqlStmt = [?EXECDB, Stmt],
    case deprecated_call(Server,{execute, SqlStmt}, Timeout) of
	{error, ErrMsg, _ErrCode} ->
	    {error, ErrMsg};
	Other ->
	    Other
    end;

erl_executeStmt(_Server, Stmt, _Timeout) ->
    exit({badarg, erl_executeStmt, {"Arg 2 is not a string", Stmt}}).

%%-------------------------DEPRECATED------------------------------------[--
%% erl_disconnect(Server, <Timeout>) -> ok | {error, ErrMsg, ErrCode}
%%	Server     = pid() | Name | {global, Name} | {Name, Node} 
%%      Timeout    = integer() | infinity
%%      ErrMsg     = string()
%%      ErrCode    = ?SQL_INVALID_HANDLE | ?SQL_ERROR
%% Description: Closes the connection to a database.
%%--------------------------------------------------------------------------
erl_disconnect(Server) ->
    erl_disconnect(Server, ?DEPRECATED_DEFAULT_TIMEOUT).

erl_disconnect(Server, Timeout) ->
    CloseStr = ?CLOSE_DB,
    Result = deprecated_call(Server,{close_connection, CloseStr}, Timeout),

    case Result of
	{error, ErrMsg, ErrCode} ->
	    {error, ErrMsg, ErrCode};
	_Other ->  %% ?SQL_SUCCESS | ?SQL_SUCCESS_WITH_INFO
	    ok
    end.

%%%-------------------------------------------------------------------------
%%% Start and Stop
%%%-------------------------------------------------------------------------

%%-----------------------DEPRECATED-----------------------------------------
%% start_link(ServerName, Args, Options) ->
%% start_link(Args, Options) ->
%%	ServerName, Args, Options - see supervisor:start_child/3
%% Description: Starts the ODBC-Erlang server and the C node.
%% Links the server to the calling process.
%% Registers the new server with the supervisor.
%%--------------------------------------------------------------------------
start_link(Args, Options) ->
    supervisor:start_child(odbc_sup, [[{client, self()} | Args], Options]).

start_link(ServerName, Args, Options) ->
    supervisor:start_child(odbc_sup,
			   [ServerName,
			    [{client, self()} | Args], Options]).

%%-----------------------DEPRECATED-----------------------------------------
%% start_link_sup(Args, Options) ->
%% start_link_sup(ServerName, Args, Options) ->
%%	ServerName, Args, Options - see gen_server:start_link[3,4]
%% Description:  Called by the supervisor to start a new server instance.
%% (start_link calls the supervisor which calls start_link_sup.)
%%--------------------------------------------------------------------------
start_link_sup(Args, Options) ->
    gen_server:start_link(?MODULE, Args, Options).

start_link_sup(ServerName, Args, Options) ->
    gen_server:start_link(ServerName, ?MODULE, Args, Options).

%%------------------------DEPRECATED----------------------------------------
%% stop(Server) -> ok
%%	Server  = pid() | Name | {global, Name} | {Name, Node} 
%% Description: Stops the ODBC-Erlang server and the C program.
%%--------------------------------------------------------------------------
stop(Server) ->
    gen_server:cast(Server, stop),
    ok.

stop(Server, _Timeout) ->  % remove later deprecated!!!
    gen_server:cast(Server, stop),
    ok.

%%----------------------DEPRECATED------------------------------------------
%% deprecated_call(Server, Msg, Timeout) -> term()
%%	Server  - pid() | Name | {global, Name} | {Name, Node} 
%%      Msg     - term()
%%      Timeout - integer() | infinity
%% Description: Sends a message to the ODBC-server and waits for
%%              the answer. It also unpacks the answer as if it is 
%%              a binary.
%%--------------------------------------------------------------------------
deprecated_call(Server, Msg = {open_connection, _ODBCCmd}, Timeout) ->
    do_call(Server, {self(),{control_cmd, Msg}}, Timeout); 
deprecated_call(Server, Msg = {close_connection, _ODBCCmd}, Timeout) ->
    do_call(Server, {self(), {control_cmd, Msg}}, Timeout); 
deprecated_call(Server, Msg, Timeout) ->
    do_call(Server, {self(), {db_cmd, Msg}}, Timeout).

do_call(Server, Msg, Timeout) ->
    Result = gen_server:call(Server, Msg, Timeout),
    if 
	binary(Result) -> % Normal case
	    binary_to_term(Result); 
	true -> % Error ocuured
	    Result
    end.

%%---------------------------DEPRECATED-------------------------------------
%% This is a simple table. The table contains tuples.
%% Each tuple contain a memoryreferens and a corrsponding
%% columnnumber.
%%--------------------------------------------------------------------------

insert({Ref, ColumnNr}, []) ->
    [{Ref, ColumnNr}];
insert({Ref, ColumnNr}, Tail) ->
    [{Ref, ColumnNr}| Tail].

lookup(_Ref, []) ->
    not_found;
lookup(Ref, [{Ref, ColumnNr} | _Tail]) ->
    ColumnNr;
lookup(Ref, [_Head | Tail] ) ->
    lookup(Ref, Tail).

delete(_Tail) ->
    [].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                END DEPRECATED INTERFACE                           %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



