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

-export([connect/2, disconnect/1, commit/2, commit/3, sql_query/2, sql_query/3,
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
		scrollable_cursors,      % on | off
		%% connecting | connected | disconnecting
		state = connecting,	    
		%% For timeout handling
		pending_request,      
		num_timeouts = 0,
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
connect(ConnectionStr, Options) when list(ConnectionStr), list(Options) ->
    
    %% Start of the odbc application should really be handled by the 
    %% application using odbc. 
    case application:start(odbc) of
	{error,{already_started,odbc}} ->
	    ok;
	ok ->
	    error_logger:info_report("The odbc application was not started."
				     " Has now been started as a temporary" 
				     " application.")
    end,
    
    %% Spawn the erlang control process.
    case supervisor:start_child(odbc_sup, [[{client, self()}]]) of
	{ok, Pid} ->
	    connect(Pid, ConnectionStr, Options);
	{error, Reason} ->
	    {error, Reason}
    end.

%%--------------------------------------------------------------------------
%% disconnect(ConnectionReferense) -> ok | {error, Reason}
%%                                    
%% Description: Disconnects from the database and terminates both the erlang
%%              control process and the database handling c-process. 
%%--------------------------------------------------------------------------
disconnect(ConnectionReference) when pid(ConnectionReference)->
    ODBCCmd = [?CLOSE_CONNECTION],
    case call(ConnectionReference, {disconnect, ODBCCmd}, 5000) of 
	{error, connection_closed} ->
	    %% If the connection has already been closed the effect of
	    %% disconnect has already been acomplished
	    ok; 
	%% Note a time out of this call will return ok, as disconnect
	%% will always succeed, the time out is to make sure
	%% the connection is killed brutaly if it will not be shut down
	%% gracefully.
	ok ->
	    ok;
	%% However you may receive an error message as result if you try to
	%% disconnect a connection started by another process.
	Other ->
	    Other
    end. 
	    
%%--------------------------------------------------------------------------
%% commit(ConnectionReference, CommitMode, <TimeOut>) ->  ok | {error, Reason} 
%%                                    
%% Description: Commits or rollbacks a transaction. Needed on connections
%%              where automatic commit is turned off.  
%%--------------------------------------------------------------------------
commit(ConnectionReference, CommitMode) ->
    commit(ConnectionReference, CommitMode, ?DEFAULT_TIMEOUT).

commit(ConnectionReference, commit, infinity) when pid(ConnectionReference) ->
    ODBCCmd = [?COMMIT_TRANSACTION, ?COMMIT],
    call(ConnectionReference, {commit, ODBCCmd}, infinity);

commit(ConnectionReference, commit, TimeOut) 
  when pid(ConnectionReference), integer(TimeOut), TimeOut > 0  ->
    ODBCCmd = [?COMMIT_TRANSACTION, ?COMMIT],
    call(ConnectionReference, {commit, ODBCCmd}, TimeOut);

commit(ConnectionReference, rollback, infinity) when pid(ConnectionReference) ->
    ODBCCmd = [?COMMIT_TRANSACTION, ?ROLLBACK],
    call(ConnectionReference, {commit, ODBCCmd}, infinity);

commit(ConnectionReference, rollback, TimeOut) 
  when pid(ConnectionReference), integer(TimeOut), TimeOut > 0  ->
    ODBCCmd = [?COMMIT_TRANSACTION, ?ROLLBACK],
    call(ConnectionReference, {commit, ODBCCmd}, TimeOut).

%%--------------------------------------------------------------------------
%% sql_query(ConnectionReference, SQLQuery, <TimeOut>) -> {updated, NRows} |
%%				{selected, ColNames, Rows} | {error, Reason} 
%%                                    
%% Description: Executes a SQL query. If it is a SELECT query the
%%              result set is returned, otherwise the number of affected 
%%       	rows are returned.
%%--------------------------------------------------------------------------
sql_query(ConnectionReference, SQLQuery) ->
    sql_query(ConnectionReference, SQLQuery, ?DEFAULT_TIMEOUT).

sql_query(ConnectionReference, SQLQuery, infinity) when 
  pid(ConnectionReference), list(SQLQuery) -> 
    ODBCCmd = [?QUERY, SQLQuery],
    call(ConnectionReference, {sql_query, ODBCCmd}, infinity);

sql_query(ConnectionReference, SQLQuery, TimeOut) 
  when pid(ConnectionReference), list(SQLQuery), integer(TimeOut), TimeOut > 0 -> 
    ODBCCmd = [?QUERY, SQLQuery],
    call(ConnectionReference, {sql_query, ODBCCmd}, TimeOut).
%%--------------------------------------------------------------------------
%% select_count(ConnectionReference, SQLQuery, <TimeOut>) -> {ok, NrRows} |
%%							     {error, Reason}  
%%                                    
%% Description: Executes a SQL SELECT query and associates the result set
%%              with the connection. A cursor is positioned before
%%        	the first row in the result set and the number of
%%	        rows in the result set is returned.
%%--------------------------------------------------------------------------
select_count(ConnectionReference, SQLQuery) ->	
    select_count(ConnectionReference, SQLQuery, ?DEFAULT_TIMEOUT).

select_count(ConnectionReference, SQLQuery, infinity) when 
  pid(ConnectionReference), list(SQLQuery) ->
    ODBCCmd = [?SELECT_COUNT, SQLQuery],
    call(ConnectionReference, {select_count, ODBCCmd}, infinity);

select_count(ConnectionReference, SQLQuery, TimeOut) when 
  pid(ConnectionReference), list(SQLQuery), integer(TimeOut), TimeOut > 0 ->
    ODBCCmd = [?SELECT_COUNT, SQLQuery],
    call(ConnectionReference, {select_count, ODBCCmd}, TimeOut).

%%--------------------------------------------------------------------------
%% first(ConnectionReference, <TimeOut>) ->  {selected, ColNames, Rows} | 
%%					     {error, Reason} 
%%                                    
%% Description: Selects the first row in the current result set. The cursor
%%            : is positioned at this row. 
%%--------------------------------------------------------------------------
first(ConnectionReference) ->	
    first(ConnectionReference, ?DEFAULT_TIMEOUT).	

first(ConnectionReference, infinity) when pid(ConnectionReference) ->	
    ODBCCmd = [?SELECT, ?SELECT_FIRST],
    call(ConnectionReference, {select_cmd, absolute, ODBCCmd}, infinity);

first(ConnectionReference, TimeOut)
  when pid(ConnectionReference), integer(TimeOut), TimeOut > 0 ->	
    ODBCCmd = [?SELECT, ?SELECT_FIRST],
    call(ConnectionReference, {select_cmd, absolute, ODBCCmd}, TimeOut).

%%--------------------------------------------------------------------------
%% last(ConnectionReference, <TimeOut>) -> {selected, ColNames, Rows} | 
%%					   {error, Reason} 
%%                                    
%% Description: Selects the last row in the current result set. The cursor
%%            : is positioned at this row. 
%%--------------------------------------------------------------------------
last(ConnectionReference) ->	
    last(ConnectionReference, ?DEFAULT_TIMEOUT).	

last(ConnectionReference, infinity) when pid(ConnectionReference) ->	
    ODBCCmd = [?SELECT, ?SELECT_LAST],
    call(ConnectionReference, {select_cmd, absolute, ODBCCmd}, infinity);

last(ConnectionReference, TimeOut) 
  when pid(ConnectionReference), integer(TimeOut), TimeOut > 0 ->	
    ODBCCmd = [?SELECT, ?SELECT_LAST],
    call(ConnectionReference, {select_cmd, absolute, ODBCCmd}, TimeOut).
%%--------------------------------------------------------------------------
%% next(ConnectionReference, <TimeOut>) -> {selected, ColNames, Rows} | 
%%					   {error, Reason}  
%%                                    
%% Description: Selects the next row relative the current cursor position 
%%            : in the current result set. The cursor is positioned at 
%%            : this row. 
%%--------------------------------------------------------------------------
next(ConnectionReference) ->	
    next(ConnectionReference, ?DEFAULT_TIMEOUT).	
    
next(ConnectionReference, infinity) when pid(ConnectionReference) ->	
    ODBCCmd = [?SELECT, ?SELECT_NEXT],
    call(ConnectionReference, {select_cmd, next, ODBCCmd}, infinity);

next(ConnectionReference, TimeOut) 
  when pid(ConnectionReference), integer(TimeOut), TimeOut > 0 ->	
    ODBCCmd = [?SELECT, ?SELECT_NEXT],
    call(ConnectionReference, {select_cmd, next, ODBCCmd}, TimeOut).

%%--------------------------------------------------------------------------
%% prev(ConnectionReference, <TimeOut>) -> {selected, ColNames, Rows} | 
%%					   {error, Reason}   
%%                                    
%% Description: Selects the previous row relative the current cursor 
%%            : position in the current result set. The cursor is
%%            : positioned at this row. 
%%--------------------------------------------------------------------------
prev(ConnectionReference) ->	
    prev(ConnectionReference, ?DEFAULT_TIMEOUT).	

prev(ConnectionReference, infinity) when pid(ConnectionReference) ->	
    ODBCCmd = [?SELECT, ?SELECT_PREV],
    call(ConnectionReference, {select_cmd, relative, ODBCCmd}, infinity);

prev(ConnectionReference, TimeOut) 
  when pid(ConnectionReference), integer(TimeOut), TimeOut > 0 ->	
    ODBCCmd = [?SELECT, ?SELECT_PREV],
    call(ConnectionReference, {select_cmd, relative, ODBCCmd}, TimeOut).

%%--------------------------------------------------------------------------
%% select(ConnectionReference, <Timeout>) -> {selected, ColNames, Rows} | 
%%					     {error, Reason}   
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

select(ConnectionReference, next, N, infinity) 
  when pid(ConnectionReference), integer(N), N > 0 ->
    ODBCCmd = [?SELECT, ?SELECT_N_NEXT,
	       integer_to_list(?DUMMY_OFFSET), ";", integer_to_list(N), ";"],
    call(ConnectionReference, {select_cmd, next, ODBCCmd},
	 infinity);

select(ConnectionReference, next, N, TimeOut) 
  when pid(ConnectionReference), integer(N), N > 0,
  integer(TimeOut), TimeOut > 0 ->
    ODBCCmd = [?SELECT, ?SELECT_N_NEXT,
	       integer_to_list(?DUMMY_OFFSET), ";", integer_to_list(N), ";"],
    call(ConnectionReference, {select_cmd, next, ODBCCmd},
	 TimeOut);

select(ConnectionReference, {relative, Pos} , N, infinity) 
  when pid(ConnectionReference), integer(Pos), Pos > 0, integer(N), N > 0 ->
    ODBCCmd = [?SELECT, ?SELECT_RELATIVE,
	       integer_to_list(Pos), ";", integer_to_list(N), ";"],
    call(ConnectionReference, {select_cmd, relative, ODBCCmd},
	 infinity);

select(ConnectionReference, {relative, Pos} , N, TimeOut) 
  when pid(ConnectionReference), integer(Pos), Pos >0, integer(N),  N > 0,
  integer(TimeOut), TimeOut > 0 ->
    ODBCCmd = [?SELECT,?SELECT_RELATIVE,
	       integer_to_list(Pos), ";", integer_to_list(N), ";"],
    call(ConnectionReference, {select_cmd, relative, ODBCCmd},
	 TimeOut);

select(ConnectionReference, {absolute, Pos} , N, infinity) 
  when pid(ConnectionReference), integer(Pos), Pos > 0, integer(N), N > 0 ->
    ODBCCmd = [?SELECT, ?SELECT_ABSOLUTE,
	       integer_to_list(Pos), ";", integer_to_list(N), ";"],
    call(ConnectionReference, {select_cmd, absolute, ODBCCmd},
	 infinity);

select(ConnectionReference, {absolute, Pos} , N, TimeOut) 
  when pid(ConnectionReference), integer(Pos), Pos > 0, integer(N),  N > 0, 
  integer(TimeOut), TimeOut > 0  ->
    ODBCCmd = [?SELECT, ?SELECT_ABSOLUTE,
	       integer_to_list(Pos), ";", integer_to_list(N), ";"],
    call(ConnectionReference, {select_cmd, absolute, ODBCCmd},
	 TimeOut).

%%%=========================================================================
%%% Start/stop
%%%=========================================================================
%%--------------------------------------------------------------------------
%% start_link_sup(Args) -> {ok, Pid} | {error, Reason} 
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
%% init(Args) -> {ok, State} | {ok, State, Timeout} | {stop, Reason}
%% Description: Initiates the erlang process that manages the connection
%%              and starts the port-program that use the odbc driver
%%		to communicate with the database.
%%-------------------------------------------------------------------------
init(Args) ->
    process_flag(trap_exit, true),
    {value, {client, ClientPid}} = lists:keysearch(client, 1, Args),
    
    erlang:monitor(process, ClientPid),
    
    %% Start the port program (a c program) that utilizes the odbc driver 
    case os:find_executable(?SERVERPROG, ?SERVERDIR) of
	FileName when list(FileName)->
	    Port  = open_port({spawn, FileName},
			      [{packet, ?LENGTH_INDICATOR_SIZE}, binary,
			       exit_status]),
	    State = #state{port = Port, owner = ClientPid},
	    {ok, State};
	false ->
	    {stop, port_program_executable_not_found}
    end.
		    
%%--------------------------------------------------------------------------
%% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State}               |
%%                                      {noreply, State, Timeout}      |
%%                                      {stop, Reason, Reply, State}   |
%%                                      {stop, Reason, Reply, State}     
%% Description: Handle incoming requests. Only requests from the process
%%              that created the connection are allowed in order to preserve
%%              the semantics of result sets.
%% Note: The order of the function clauses is significant.
%%--------------------------------------------------------------------------
handle_call({Client, Msg, Timeout}, From, State = 
	    #state{owner = Client, reply_to = undefined})  ->
    handle_msg(Msg, Timeout, State#state{reply_to = From});

%% The client has caught the timeout and is sending a new
%% request, but we must preserve a synchronous communication with the port.
%% This request will be handled when we have received the answer to the
%% timed out request and thrown it away, if it has not already been timed out
%% itself in which case the request is thrown away.
handle_call(Request = {Client, Msg, Timeout}, From, 
	    State = #state{owner = Client, reply_to = skip,
			   num_timeouts = N}) when N < ?MAX_SEQ_TIMEOUTS ->
    {noreply, State#state{pending_request = {Request, From}}, Timeout};

%% The client has sent so many sequential requests that has timed out that 
%% there might be something radically wrong causing the ODBC-driver to
%% hang. So we give up and close the connection. 
handle_call(Request = {Client, Msg, Timeout}, From, 
	    State = #state{owner = Client,  
			   num_timeouts = N}) when N >= ?MAX_SEQ_TIMEOUTS ->
    gen_server:reply(From, {error, connection_closed}), 
    {stop, too_many_sequential_timeouts, State#state{reply_to = undefined}};

handle_call(_, _, State) ->
    {reply, {error, process_not_owner_of_odbc_connection}, 
     State#state{reply_to = undefined}}.

%%--------------------------------------------------------------------------
%% Func: handle_msg(Msg, Timeout, State) -> same as handle_call/3.
%% Description: Sends requests to the port-program.
%% Note: The order of the function clauses is significant.
%%--------------------------------------------------------------------------
handle_msg({connect, ODBCCmd, AutoCommitMode, SrollableCursors},
	   Timeout, State) ->
    port_command(State#state.port, [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, State#state{auto_commit_mode = AutoCommitMode,
			  scrollable_cursors = SrollableCursors}, Timeout};

handle_msg({disconnect, ODBCCmd}, Timeout, State) ->
    port_command(State#state.port,  [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, State#state{state = disconnecting}, Timeout};

handle_msg({commit, _ODBCCmd}, Timeout, 
	   State = #state{auto_commit_mode = on}) ->
    {reply, {error, not_an_explicit_commit_connection}, 
     State#state{reply_to = undefined}, Timeout};

handle_msg({commit, ODBCCmd}, Timeout, 
	   State = #state{auto_commit_mode = off}) ->
    port_command(State#state.port,  [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, State, Timeout};

handle_msg({sql_query, ODBCCmd}, Timeout, State) ->
    port_command(State#state.port,  [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, State#state{result_set = undefined}, Timeout};

handle_msg({select_count, ODBCCmd}, Timeout, State) ->
    port_command(State#state.port,  [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, State#state{result_set = exists}, Timeout};

handle_msg({select_cmd, absolute, ODBCCmd}, Timeout,
	   State = #state{result_set = exists, absolute_pos = true}) ->
    port_command(State#state.port,  [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, State, Timeout};

handle_msg({select_cmd, relative, ODBCCmd}, Timeout, 
	   State = #state{result_set = exists, relative_pos = true}) ->
    port_command(State#state.port,  [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, State, Timeout};

handle_msg({select_cmd, next, ODBCCmd}, Timeout,
	   State = #state{result_set = exists}) ->
    port_command(State#state.port,  [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, State, Timeout};

handle_msg({select_cmd, _Type, _ODBCCmd}, _Timeout,
	   State = #state{result_set = undefined}) ->
    {reply, {error, result_set_does_not_exist}, 
     State#state{reply_to = undefined}};

handle_msg({select_cmd, _Type, _ODBCCmd}, _Timeout, State) ->
    Reply = case State#state.scrollable_cursors of
		on ->
		    {error, driver_does_not_support_function};
		off ->
		    {error, scrollable_cursors_disabled}
	    end,
	    
    {reply, Reply, State#state{reply_to = undefined}};

%%%%%%%%%%%%%%%% DEPRECATED CLAUSES OF HANDLE_MSG START %%%%%%%%%%%%%%%%%%%%
%-------------------------DEPRECATED---------------------------------------
%% Open connection to database
handle_msg({control_cmd, {open_connection, ODBCCmd}}, Timeout, 
	    State = #state{connected = false}) ->
    Port  = State#state.port,
    port_command(Port,  [ODBCCmd, ?STR_TERMINATOR]),
    NewState = State#state{connected = true, 
			   %% Not entirely true but needed so this old
			   %% interface will still work as before
			   state = connected},
    {noreply, NewState, Timeout};

%-------------------------DEPRECATED----------------------------------------
%% If you try opening a connection that is already open
handle_msg({control_cmd, {open_connection, _ODBCCmd}}, _Timeout, 
	    State = #state{connected = true}) ->
    {reply, {error, "Connection already open", ?SQL_ERROR}, 
     State#state{reply_to = undefined}};
%-------------------------DEPRECATED----------------------------------------
%% Close connection to database
handle_msg({control_cmd, {close_connection, ODBCCmd}}, Timeout,
	    State = #state{connected = true}) ->
    Port = State#state.port,
    NewState = State#state{connected = false},
    port_command(Port,  [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, NewState, Timeout};
%-------------------------DEPRECATED----------------------------------------
%% If you try closing a connection that is already closed
handle_msg({control_cmd, {close_connection, _ODBCCmd}}, _Timeout,
	   State = #state{connected = false}) ->
    {reply, {error, "Connection already closed", ?SQL_ERROR}, 
     State#state{reply_to = undefined}};
%-------------------------DEPRECATED----------------------------------------
%---------------------------------------------------------------------------
%% ODBC commands - require that we have a connection to the database
handle_msg({db_cmd, _Cmd}, _Timeout,
            #state{connected = false} = State) ->
    {reply, {error, "Not connected", ?SQL_ERROR}, 
     State#state{reply_to = undefined}};
%-------------------------DEPRECATED----------------------------------------
handle_msg({db_cmd, {bind_column, ODBCCmd, ColNum, MemRef}}, Timeout,
	   State = #state{connected = true}) ->
    Port = State#state.port,
    ColumnVec = State#state.columnvector,
    NewColumnVec = insert({MemRef,ColNum}, ColumnVec),
    NewState = State#state{columnvector = NewColumnVec},
    NewODBCCmd = [ODBCCmd, integer_to_list(ColNum)],
    port_command(Port,  [NewODBCCmd, ?STR_TERMINATOR]),
    {noreply, NewState, Timeout};
%-------------------------DEPRECATED----------------------------------------
handle_msg({db_cmd,{close_handle, ODBCCmd}}, Timeout, State =
	   #state{connected = true}) ->
    Port = State#state.port,
    ColumnVect    = State#state.columnvector,
    NewColumnVect = delete(ColumnVect),
    NewState = State#state{columnvector = NewColumnVect, fetchdata = false},
    port_command(Port,  [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, NewState, Timeout};
%-------------------------DEPRECATED----------------------------------------
handle_msg({db_cmd, {read_buffer, ODBCCmd, ColumnRef}}, Timeout, 
	    State = #state{connected = true, fetchdata = true}) ->
    Port       = State#state.port,
    ColumnVect = State#state.columnvector,
    CNumber    = lookup(ColumnRef,ColumnVect),

    case CNumber of
	not_found ->
	    {reply, {error, column_not_found, ?SQL_ERROR}, 
	     State#state{reply_to = undefined}};
	_ ->
	    NewODBCCmd = [ODBCCmd, integer_to_list(CNumber)],
	    port_command(Port,  [NewODBCCmd, ?STR_TERMINATOR]),
	    {noreply, State, Timeout}
    end;
%-------------------------DEPRECATED----------------------------------------
handle_msg({db_cmd, {read_buffer, _ODBCCmd, _ColumnRef}}, _Timeout, 
	    #state{fetchdata = false} = State) ->
    {reply, {error, no_data_fetched, ?SQL_ERROR}, 
     State#state{reply_to = undefined}};
%-------------------------DEPRECATED----------------------------------------
handle_msg({db_cmd, {fetch, ODBCCmd}}, Timeout, State = 
	    #state{connected = true}) ->
    Port = State#state.port,
    port_command(Port,  [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, State#state{fetchdata = true}, Timeout};
%-------------------------DEPRECATED----------------------------------------
%% Tag = execute | execdir | describeColumn | endTran | 
%% | numResultCols | rowCount | setConnectAttr 
handle_msg({db_cmd, {_Tag, ODBCCmd}}, Timeout, State = 
	   #state{connected = true}) ->
    Port = State#state.port,
    port_command(Port,  [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, State, Timeout};

%%%%%%%%%%%%%%%% DEPRECATED CLAUSES OF HANDLE_MSG END %%%%%%%%%%%%%%%%%%%%%%

%---------------------------------------------------------------------------
%% Catch all -  This can oly happen if the application programmer writes 
%% really bad code that violates the API.
handle_msg(Request, _Timeout, State) ->
    {stop, {'API_violation_connection_colsed', Request},
     {error, connection_closed}, State#state{reply_to = undefined}}.

%%--------------------------------------------------------------------------
%% handle_cast(Request, State) -> {noreply, State} | {noreply, State, Timeout}|
%%                                {stop, Reason, State} 
%% Description:         
%% Note: The order of the function clauses is significant.
%%-------------------------------------------------------------------------
%% Used if the odbc connect call fails as this is discover in the client-code.
%% see function connect/3
handle_cast(stop, State) ->
    {stop, normal, State};

%% Send debug request to the port-program.
handle_cast({debugc, DebugCmd}, State) ->
    port_command(State#state.port, [DebugCmd, ?STR_TERMINATOR]),
    {noreply, State};

%% Catch all - This can only happen if the application programmer writes 
%% really bad code that violates the API.
handle_cast(Msg, State) ->
    {stop, {'API_violation_connection_colsed', Msg}, State}.

%%--------------------------------------------------------------------------
%% handle_info(Msg, State) -> {noreply, State} | {noreply, State, Timeout} |
%%			      {stop, Reason, State}
%% Description: Handles timouts, replys from the port-program and EXIT and
%%		down messages.
%% Note: The order of the function clauses is significant.
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
	    {stop, normal, State#state{reply_to = undefined}}
    end;

handle_info({Port, {data, BinData}}, 
	    State = #state{state = connected,
 			   port = Port,
 			   reply_to = skip,
 			   pending_request = undefined}) ->
    %% Disregard this message as it is a answer to a query that has timed
    %% out.
    {noreply, State#state{reply_to = undefined}};

handle_info({Port, {data, BinData}},
 	    State = #state{state = connected, port = Port,
 			   reply_to = skip}) ->
    
    %% Disregard this message as it is a answer to a query that has timed
    %% out and process the pending request. 
    {{Client, Msg, Timeout}, From} = State#state.pending_request,
    handle_msg(Msg, Timeout, State#state{pending_request=undefined,
					 reply_to = From});

handle_info({Port, {data, BinData}}, State = #state{state = connected,
						    reply_to = From,
 						    port = Port}) ->
    %% Send the reply from the database (received by the erlang control 
    %% process from the port program) to the waiting client.
    gen_server:reply(From, BinData),
    {noreply, State#state{reply_to = undefined,
			  num_timeouts = 0}};

handle_info({Port, {data, BinData}},  State = #state{state = disconnecting,
						     reply_to = From,
 						     port = Port}) ->

    %% The connection will always be closed 
    gen_server:reply(From, ok),  
    
    case binary_to_term(BinData) of
 	ok -> 
 	    ok;
 	{error, Reason} ->
 	    error_logger:error_report("ODBC could not end connection "  
 				      "gracefully due to ~p~n", [Reason])
    end,
    
    {stop, normal, State#state{reply_to = undefined}};

handle_info(timeout, State = #state{state = disconnecting, 
 				    reply_to = From}) when From /= undefined ->
    gen_server:reply(From, ok), 
    {stop, {timeout, "Port program is not responding to disconnect, " 
 	    "will be killed"}, State};

handle_info(timeout, State = #state{state = connecting, 
 				    reply_to = From}) when From /= undefined ->
    gen_server:reply(From, timeout),
    {stop, normal, State#state{reply_to = undefined}};

handle_info(timeout, State = #state{state = connected, 
				    pending_request = undefined,
				    reply_to = From}) when From /= undefined ->
    gen_server:reply(From, timeout),
    {noreply, State#state{reply_to = skip,
			  num_timeouts = State#state.num_timeouts + 1}};

handle_info(timeout, State =
	    #state{state = connected,      
		   pending_request = {{_, {disconnect, _}, _}, 
				      PendingFrom}}) ->
    gen_server:reply(PendingFrom, ok),
    {stop, {timeout, "Port-program busy when trying to disconnect,  "
	    "will be killed"},
     State#state{pending_request = undefined, reply_to = undefined,
		 num_timeouts = State#state.num_timeouts + 1}};

handle_info(timeout, State =
	    #state{state = connected, 
		   pending_request = {Request, PendingFrom}}) ->
    gen_server:reply(PendingFrom, timeout),
    %% The state variable reply_to should continue to have the value skip 
    {noreply, State#state{pending_request = undefined,  
 			  num_timeouts = State#state.num_timeouts + 1}};

handle_info({Port, {exit_status, Status}}, State = #state{port = Port}) ->
    case State#state.reply_to of
 	undefined ->
 	    ok;
 	From ->
 	    gen_server:reply(From, {error, connection_closed})
    end,
    {stop, {exit_status, Status}, State#state{reply_to = undefined}};


handle_info({'EXIT', Port, Reason}, State = #state{port = Port}) ->
    case State#state.reply_to of
	undefined ->
	    ok;
	From ->
	    gen_server:reply(From, {error, connection_closed})
    end,
    NewReason = receive 
		    {Port, {exit_status, Status}} -> 
			{port_exit, Status}
		after 5000 ->
			{port_exit, Reason}
		end,
    {stop, NewReason, State#state{reply_to = undefined}};

%%% If the owning process dies there is no reson to go on
handle_info({'DOWN', _Ref, _Type, Process, normal}, State) ->
    {stop, normal, State#state{reply_to = undefined}};
    
handle_info({'DOWN', _Ref, _Type, Process, timeout}, State) ->
    {stop, normal, State#state{reply_to = undefined}};
 
handle_info({'DOWN', _Ref, _Type, Process, Reason}, State) ->
    {stop, {stopped, {'EXIT', Process, Reason}}, 
     State#state{reply_to = undefined}};
    
%---------------------------------------------------------------------------
%% Catch all - throws away unknown messages (This could happen by "accident"
%% so we do not want to crash, but we make a log entry as it is an
%% unwanted behaviour.) 
handle_info(Info, State) ->
    error_logger:error_report("ODBC: received unexpected info: ~p~n", [Info]),
    {noreply, State}.

%%--------------------------------------------------------------------------
%% terminate/2 and code_change/3
%%--------------------------------------------------------------------------

terminate({port_exit, _Reason}, #state{reply_to = undefined}) ->
    ok;

terminate(_Reason,  State = #state{reply_to = undefined}) ->
    Port = State#state.port,
    port_close(Port),
    receive
	{Port, closed} ->
	    ok;
	{'EXIT', Port, normal} ->
	    ok
    after 3000 ->
	    error_logger:error_report("Erlang ODBC-process did not receive "
				      "closed port message.")
    end;

terminate(Reason, State = #state{reply_to = From}) ->
    gen_server:reply(From, {error, connection_closed}),
    terminate(Reason, State#state{reply_to = undefined}).

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
    {C_SrollableCursors, ERL_SrollableCursors} = 
	connection_config(scrollable_cursors, Options),
    {C_TupleRow, ERL_tupleRow} = 
	connection_config(tuple_row, Options),
    ODBCCmd = 
	[?OPEN_CONNECTION, C_AutoCommitMode, C_TraceDriver, 
	 C_SrollableCursors, C_TupleRow, ConnectionStr],
    
    %% Send request, to open a database connection, to the control process.
    case call(ConnectionReferense, 
	      {connect, ODBCCmd, ERL_AutoCommitMode, ERL_SrollableCursors},
	      TimeOut) of
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

connection_default(tuple_row) ->
  {?OFF, off};

connection_default(trace_driver) ->
    {?OFF, off};

connection_default(scrollable_cursors) ->
    {?ON, on}.

%%-------------------------------------------------------------------------
call(ConnectionReference, Msg, Timeout) ->
    
    Result = (catch gen_server:call(ConnectionReference, 
				    {self(), Msg, Timeout}, infinity)),
    case Result of
	%% Normal case, the result from the port-program has directly 
	%% been forwarded to the client
	Binary when binary(Binary) -> 
	    binary_to_term(Binary); 
	timeout -> 
	    exit(timeout);
	{'EXIT', _} ->
	    {error, connection_closed};
	Other ->  % Special case or error
	    Other
    end.    
%%-------------------------------------------------------------------------
cast(ConnectionReference, Msg) ->
    gen_server:cast(ConnectionReference, Msg).

%%%========================================================================
%%% Debug functions
%%%========================================================================

%%--------------------------------------------------------------------------
%% debugerl(Process, OnOff, <Level>) -> ok
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

%%-------------------------DEPRECATED--------------------------------------
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
    do_call(Server, {self(),{control_cmd, Msg}, Timeout}); 
deprecated_call(Server, Msg = {close_connection, _ODBCCmd}, Timeout) ->
    do_call(Server, {self(), {control_cmd, Msg}, Timeout}); 
deprecated_call(Server, Msg, Timeout) ->
    do_call(Server, {self(), {db_cmd, Msg}, Timeout}).

do_call(Server, Msg) ->
    Result = (catch gen_server:call(Server, Msg, infinity)),
    case Result of
	%% Normal case, the result from the port-program has directly 
	%% been forwarded to the client
	Binary when binary(Binary) -> 
	    binary_to_term(Binary); 
	timeout -> 
	    exit(timeout);
	{'EXIT',{noproc, _}} ->
	    {error, connection_closed};
	Other ->  % Special case or error
	    Other
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
