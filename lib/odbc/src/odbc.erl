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


%% External exports------------------------------------
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

%%------------------------------------------------------
%% Internal exports (for supervisor).
-export([start_link_sup/2, start_link_sup/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
	 terminate/2, code_change/3]).

%% debuge start/stop debugging of erlang
-export([debugerl/2, debugerl/3]).
%%  debugc start/stop debugging of c-program 
-export([debugc/2]).

%% Internal state
-record(state, {port,                        % The port to the c-program
%		wait_for_port = false,
		connected = false,           % Connected to database
		fetchdata = false,           % fetched data before read
		reply_to,			     % 
	%	clientpid,                   % Client Pid
		columnvector = [],            % memoryref for columns
		queue = queue:new()
	       }).

%%------------------------------------------------------
%% Internal macros
%% Path to the c-program.
-define(SERVERDIR, filename:nativename(
		     filename:join(code:priv_dir(odbc), "bin"))).

%% Name of the C program 
-define(SERVERPROG, "odbcserver").

%% Other constants
-define(DEFAULT_TIMEOUT, 10000). % 10 sec
-define(STR_TERMINATOR, 0).
-define(SQL_ERROR, -1). %FOO

%% Constats defining the command protocol between the erlang control process
%% and the port program. These constants must also be defined in the same way 
%% in the port program.
-define(OPEN_DB, 1).
-define(CLOSE_DB, 2).
-define(BIND_COLUMN, 3).
-define(DESCRIBE_COLUMN, 4).
-define(END_TRANSACTION, 5).
-define(EXEDIR, 6).
-define(FETCH_DATA, 7).
-define(NUMBER_RESULT_COLUMNS, 8).
-define(ROW_COUNT, 9).
-define(SET_ATTRIBUTE, 10).
-define(READ_BUFFER, 11).
-define(CLOSE_HANDLE, 12).
-define(EXECDB, 13).
-define(DEBUG, 14).
-define(LENGTH_INDICATOR_SIZE, 4).
-define(INT_VALUE, 1).
-define(STR_VALUE, 2).
-define(DEBUG_ON, 1).
-define(DEBUG_OFF, 2).


%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% These functions are deprecated and is included only for 
%% compatibility reason. Some of them do nothing and some 
%% are replaced by new function.
%% Deprecated functions
-export([sqlCloseCursor/1, sqlCloseCursor/2]).
-export([sql_alloc_handle/3, sql_alloc_handle/4]).
-export([sql_bind_col/4, sql_bind_col/5]).
-export([sql_close_cursor/2, sql_close_cursor/3]).
-export([sql_connect/5, sql_connect/6]).
-export([sql_describe_col/4, sql_describe_col/5]).
-export([sql_disconnect/2, sql_disconnect/3]).
-export([sql_driver_connect/5, sql_driver_connect/6]).
-export([sql_end_tran/4, sql_end_tran/5]).
-export([sql_exec_direct/3, sql_exec_direct/4]).
-export([sql_fetch/2, sql_fetch/3]).
-export([sql_free_handle/3, sql_free_handle/4]).
-export([sql_get_connect_attr/4, sql_get_connect_attr/5]).
-export([sql_get_diag_rec/5, sql_get_diag_rec/6]).
-export([sql_num_result_cols/2, sql_num_result_cols/3]).
-export([sql_row_count/2, sql_row_count/3]).
-export([sql_set_connect_attr/5, sql_set_connect_attr/6]).
-export([sql_set_env_attr/5, sql_set_env_attr/6]).
-export([alloc_buffer/3, alloc_buffer/4]).
-export([dealloc_buffer/2, dealloc_buffer/3]).
-export([read_buffer/2, read_buffer/3]).
-export([init_env/1, init_env/2]).
-export([connect/3, connect/4, connect/5, connect/6]).
-export([execute_stmt/3, execute_stmt/4]).
-export([disconnect/2, disconnect/3]).
-export([terminate_env/2, terminate_env/3]).
-export([display_size/2]).
%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

%%%----------------------------------------------------------
%%% Start and Stop
%%%----------------------------------------------------------

%%---------------------------------------------------------------------------
%% start_link(ServerName, Args, Options) ->
%% start_link(Args, Options) ->
%%	ServerName, Args, Options - see supervisor:start_child/3
%% Description: Starts the ODBC-Erlang server and the C node.
%% Links the server to the calling process.
%% Registers the new server with the supervisor.
%%-------------------------------------------------------------------------
start_link(Args, Options) ->
    supervisor:start_child(odbc_sup, [[{client, self()} | Args], Options]).

start_link(ServerName, Args, Options) ->
    supervisor:start_child(odbc_sup,
			   [ServerName, [{client, self()} | Args], Options]).

%%---------------------------------------------------------------------------
%% start_link_sup(Args, Options) ->
%% start_link_sup(ServerName, Args, Options) ->
%%	ServerName, Args, Options - see gen_server:start_link[3,4]
%% Description:  Called by the supervisor to start a new server instance.
%% (start_link calls the supervisor which calls start_link_sup.)
%%-------------------------------------------------------------------------
start_link_sup(Args, Options) ->
    gen_server:start_link(?MODULE, Args, Options).

start_link_sup(ServerName, Args, Options) ->
    gen_server:start_link(ServerName, ?MODULE, Args, Options).

%%---------------------------------------------------------------------------
%% stop(Server) -> ok
%%	Server  = pid() | Name | {global, Name} | {Name, Node} 
%% Description: Stops the ODBC-Erlang server and the C program.
%%-------------------------------------------------------------------------
stop(Server) ->
    gen_server:cast(Server, stop),
    ok.

stop(Server, _Timeout) ->  % remove later deprecated!!!
    gen_server:cast(Server, stop),
    ok.

%%%======================================================================
%%% Basic API
%%%======================================================================

%%---------------------------------------------------------------------------
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
%%---------------------------------------------------------------------------
sqlBindColumn(Server, ColNum, Ref) ->
    sqlBindColumn(Server, ColNum, Ref, ?DEFAULT_TIMEOUT).

sqlBindColumn(Server, ColNum, Ref, Timeout)
  when integer(ColNum), reference(Ref) ->
 
    ODBCCmd = ?BIND_COLUMN,   
    call(Server,{bind_column, ODBCCmd, ColNum, Ref}, Timeout);

sqlBindColumn(_Server, ColNum, Ref, _Timeout) when integer(ColNum) ->
    exit({badarg, sqlBindColumn, {"Arg 3 is not a reference", Ref}});

sqlBindColumn(_Server, ColNum, Ref, _Timeout) ->
    exit({badarg, sqlBindColumn, {"Arg 2 is not an integer", ColNum}}).
%%---------------------------------------------------------------------------
%% sqlCloseHandle(Server, <Timeout>) -> Result | {error, ErrMsg, ErrCode} 
%%	Server  = pid() | Name | {global, Name} | {Name, Node} 
%%      Timeout = integer() | infinity
%%      Result  = ?SQL_SUCCESS | ?SQL_SUCCESS_WITH_INFO
%%      ErrMsg   = string()
%%      ErrCode  = ?SQL_INVALID_HANDLE | ?SQL_ERROR 
%% Description: Close Stament handle. CloseHandle is needed when basic SQL 
%%              functions are used.
%%---------------------------------------------------------------------------
sqlCloseHandle(Server) ->
    sqlCloseCursor(Server, ?DEFAULT_TIMEOUT).

sqlCloseHandle(Server, Timeout) ->
    ODBCCmd = ?CLOSE_HANDLE, 
    call(Server, {close_handle, ODBCCmd}, Timeout).

%%---------------------------------------------------------------------------
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
%%---------------------------------------------------------------------------
sqlConnect(Server, DSN, UID, PWD) ->
    sqlConnect(Server, DSN, UID, PWD, ?DEFAULT_TIMEOUT).

sqlConnect(Server, DSN, UID, PWD, Timeout) 
  when list(DSN), list(UID), list(PWD) ->
    ODBCCmd =  [?OPEN_DB, "DSN=",DSN,";UID=",UID,";PWD=",PWD],
    call(Server, {open_connection, ODBCCmd}, Timeout);

sqlConnect(_Server, DSN, UID, PWD, _Timeout) 
   when list(DSN), list(UID) ->
     exit({badarg, sqlConnect, {"Arg 4 is not a string", PWD}});    
 sqlConnect(_Server, DSN, UID, _PWD, _Timeout) 
   when list(DSN) ->
     exit({badarg, sqlConnect, {"Arg 3 is not a string", UID}});
 sqlConnect(_Server, DSN, _UID, _PWD, _Timeout) ->
     exit({badarg, sqlConnect, {"Arg 2 is not a string", DSN}}).
%%---------------------------------------------------------------------------
%% sqlDescribeCol(Server, ColNum, <Timeout>) -> {Result, ColName, Nullable} |
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
%%---------------------------------------------------------------------------
sqlDescribeCol(Server, ColNum) ->
    sqlDescribeCol(Server, ColNum, ?DEFAULT_TIMEOUT).

sqlDescribeCol(Server, ColNum, Timeout) when integer(ColNum) ->
    ODBCCmd = [?DESCRIBE_COLUMN, integer_to_list(ColNum)],
    call(Server, {describeColumn, ODBCCmd}, Timeout);

sqlDescribeCol(_Server, ColNum, _Timeout) ->
    exit({badarg, sqlDescribeCol, {"Arg 2 is not an integer", ColNum}}).    

%%---------------------------------------------------------------------------
%% sqlDisconnect(Server, <Timeout>) -> Result | {error, ErrMsg, ErrCode} 
%%	Server  = pid() | Name | {global, Name} | {Name, Node} 
%%      Timeout = integer() | infinity
%%      Result  = ?SQL_SUCCESS | ?SQL_SUCCESS_WITH_INFO
%%      ErrMsg   = string()
%%      ErrCode  = ?SQL_INVALID_HANDLE | ?SQL_ERROR 
%% Description: Closes a database connection.
%%--------------------------------------------------------------------------
sqlDisConnect(Server) ->
    sqlDisConnect(Server, ?DEFAULT_TIMEOUT).

sqlDisConnect(Server, Timeout) ->
    CloseStr = ?CLOSE_DB,
    call(Server,{close_connection, CloseStr}, Timeout).

%%---------------------------------------------------------------------------
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
%%		connection. It can only request a commit or rollback operation
%%	        for a single connection.
%%--------------------------------------------------------------------------
sqlEndTran(Server, ComplType) ->
    sqlEndTran(Server, ComplType, ?DEFAULT_TIMEOUT).

sqlEndTran(Server, ComplType, Timeout) ->
    ODBCCmd = [?END_TRANSACTION, ComplType],
    call(Server,{endTran, ODBCCmd}, Timeout).

%%---------------------------------------------------------------------------
%% sqlExecDirect(Server, Stmt, <Timeout>) -> Result | {error, ErrMsg, ErrCode} 
%%	Server  = pid() | Name | {global, Name} | {Name, Node} 
%%      Timeout = integer() | infinity
%%      Result  = ?SQL_SUCCESS | ?SQL_SUCCESS_WITH_INFO
%%      ErrMsg   = string()
%%      ErrCode  = ?SQL_INVALID_HANDLE | ?SQL_ERROR 
%% Description: 
%%--------------------------------------------------------------------------
sqlExecDirect(Server, Stmt) ->
    sqlExecDirect(Server, Stmt, ?DEFAULT_TIMEOUT).

sqlExecDirect(Server, Stmt, Timeout) when list(Stmt) ->
    CmdStmt = [?EXEDIR, Stmt],
    call(Server, {execdir,CmdStmt}, Timeout);

sqlExecDirect(_Server, Stmt, _Timeout) ->
    exit({badarg, sqlExecDirect, {"Arg 2 is not a string", Stmt}}).    

%%---------------------------------------------------------------------------
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
    sqlFetch(Server, ?DEFAULT_TIMEOUT).

sqlFetch(Server, Timeout) ->
    ODBCCmd = ?FETCH_DATA, 
    call(Server,{fetch, ODBCCmd}, Timeout).

%%---------------------------------------------------------------------------
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
    sqlNumResultCols(Server, ?DEFAULT_TIMEOUT).

sqlNumResultCols(Server, Timeout) ->
    ODBCCmd = ?NUMBER_RESULT_COLUMNS,
    call(Server,{numResultCols, ODBCCmd}, Timeout).

%%---------------------------------------------------------------------------
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
    sqlRowCount(Server, ?DEFAULT_TIMEOUT).

sqlRowCount(Server, Timeout) ->
    ODBCCmd = ?ROW_COUNT,
    call(Server,{rowCount, ODBCCmd},Timeout).
    
%%---------------------------------------------------------------------------
%% sqlSetConnectAttr(Server, Attr, Value, <Timeout>) ->  Result |
%%					              {error, ErrMsg, ErrCode} 
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
    sqlSetConnectAttr(Server, Attr, Value, ?DEFAULT_TIMEOUT).

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
    
    call(Server, {setConnectAttr, ODBCCmd}, Timeout).

%%---------------------------------------------------------------------------
%% readData(Server, Ref, <Timeout>) ->  {ok, value} | {error, ErrMsg, ErrCode}
%%	Server  = pid() | Name | {global, Name} | {Name, Node} 
%%      Ref     = ref()
%%      Timeout = integer() | infinity
%%      Result  = ?SQL_SUCCESS | ?SQL_SUCCESS_WITH_INFO
%%      ErrMsg  = string()
%%      ErrCode = ?SQL_INVALID_HANDLE | ?SQL_ERROR 
%% Description: Returns data from the sqlFetch/[1,2] call.
%%--------------------------------------------------------------------------
readData(Server, Ref) ->
    readData(Server, Ref, ?DEFAULT_TIMEOUT).

readData(Server, Ref, Timeout) ->
    ODBCCmd = ?READ_BUFFER,
    call(Server, {read_buffer, ODBCCmd, Ref}, Timeout).

%%---------------------------------------------------------------------------
%% columnRef() -> ref()
%% Description: Generate a referens for a column. 
%%---------------------------------------------------------------------------
columnRef() ->
    Ref = erlang:make_ref(),
    {ok, Ref}.

%%%======================================================================
%%% Utility API
%%%======================================================================

%%---------------------------------------------------------------------------
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
%% Description: Opens a connection to a database. There can be only one open 
%%              database connection per server.
%%--------------------------------------------------------------------------
erl_connect(Server, ConnectStr) ->
    erl_connect(Server, ConnectStr, ?DEFAULT_TIMEOUT).

erl_connect(Server, ConnectStr, Timeout) when list(ConnectStr) ->
    ODBCCmd = [?OPEN_DB, ConnectStr],
    Result = call(Server,{open_connection, ODBCCmd}, Timeout),

    case Result of
	{error, ErrMsg, ErrCode} ->
	    {error,ErrMsg, ErrCode};
	Other -> %% ?SQL_SUCCESS | ?SQL_SUCCESS_WITH_INFO
	    ok
    end;

erl_connect(_Server, ConnectStr, _Timeout) ->
    exit({badarg, erl_connect, {"Arg 2 is not a string", ConnectStr}}).    

%%---------------------------------------------------------------------------
%% erl_connect(Server, DSN, UID, PWD, <Timeout>) ->  ok, | 
%%                                                 {error, ErrMsg, ErrCode} 
%%	Server  = pid() | Name | {global, Name} | {Name, Node} 
%%      DNS     = string() - The name of the database.
%%      UID     = string() - The user ID.
%%      PWD     = string() - The user's password for the database.
%%      Timeout = integer() | infinity
%%      ErrMsg     = string()
%%      ErrCode    = ?SQL_INVALID_HANDLE | ?SQL_ERROR
%% Description: Opens a connection to a database. There can be only one open 
%%              database connection per server.
%%-------------------------------------------------------------------------
erl_connect(Server, DSN, UID, PWD) ->
    erl_connect(Server, DSN, UID, PWD, ?DEFAULT_TIMEOUT).

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

%%---------------------------------------------------------------------------
%% erl_executeStmt(Server, Stmt, <Timeout>) -> {updated, NRows} | 
%%                                             {selected, [ColName], [Row]} | 
%%                                             {error, ErrMsg}
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
    erl_executeStmt(Server, Stmt, ?DEFAULT_TIMEOUT).

erl_executeStmt(Server, Stmt, Timeout) when list(Stmt) ->
    SqlStmt = [?EXECDB, Stmt],
    case call(Server,{execute, SqlStmt}, Timeout) of
	{error, ErrMsg, ErrCode} ->
	    {error, ErrMsg};
	Other ->
	    Other
    end;

erl_executeStmt(_Server, Stmt, _Timeout) ->
    exit({badarg, erl_executeStmt, {"Arg 2 is not a string", Stmt}}).

%%---------------------------------------------------------------------------
%% erl_disconnect(Server, <Timeout>) -> ok | {error, ErrMsg, ErrCode}
%%	Server     = pid() | Name | {global, Name} | {Name, Node} 
%%      Timeout    = integer() | infinity
%%      ErrMsg     = string()
%%      ErrCode    = ?SQL_INVALID_HANDLE | ?SQL_ERROR
%% Description: Closes the connection to a database.
%%--------------------------------------------------------------------------
erl_disconnect(Server) ->
    erl_disconnect(Server, ?DEFAULT_TIMEOUT).

erl_disconnect(Server, Timeout) ->
    CloseStr = ?CLOSE_DB,
    Result = call(Server,{close_connection, CloseStr}, Timeout),

    case Result of
	{error, ErrMsg, ErrCode} ->
	    {error, ErrMsg, ErrCode};
	Other ->  %% ?SQL_SUCCESS | ?SQL_SUCCESS_WITH_INFO
	    ok
    end.

%%%======================================================================
%%% Callback functions from gen_server
%%%======================================================================

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          {stop, Reason}
%%----------------------------------------------------------------------
init(Args) ->
    process_flag(trap_exit, true),
    {value, {client, ClientPid}} = lists:keysearch(client, 1,Args),

    link(ClientPid),
    
    %% Start the C program
    case os:find_executable(?SERVERPROG, ?SERVERDIR) of
	FileName when list(FileName)->
	    Port  = open_port({spawn, FileName},
			      [{packet, ?LENGTH_INDICATOR_SIZE}, binary]),
	    State = #state{port = Port}, %, clientpid = ClientPid},
	    {ok, State};
	false ->
	    {stop, "Can't find the port-program: odbcserver executable."}
    end.
		    
%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, Reply, State}     (terminate/2 is called)
%%----------------------------------------------------------------------
handle_call(Msg, From, State = #state{reply_to = undefined}) ->
    case queue:out(State#state.queue) of
 	{empty, Queue} ->
 	    handle_msg(Msg, From, State);
 	{{value, {WaitingMsg, WaitingFrom}}, Queue}  ->
	    NewQueue = queue:in({Msg, From}, Queue),
	    NewState = State#state{queue = NewQueue},
	    handle_msg(WaitingMsg, WaitingFrom, NewState)
    end;
handle_call(Msg, From, State) ->
    NewQueue = queue:in({Msg, From}, State#state.queue),
    NewState = State#state{queue = NewQueue},
    {noreply, NewState}.

%%----------------------------------------------------------------------
%% Func: handle_msg/3 - (help function to handle_call/3)
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, Reply, State}     (terminate/2 is called)
%% Description: 
%%----------------------------------------------------------------------
%% Open connection to database
handle_msg({control_cmd, {open_connection, ODBCCmd}}, From, 
	    State = #state{connected = false}) ->
    Port  = State#state.port,
    port_command(Port,  [ODBCCmd, ?STR_TERMINATOR]),
    NewState = State#state{connected = true, reply_to = From},
    {noreply, NewState};

%% If you try opening a connection that is already open
handle_msg({control_cmd, {open_connection, ODBCCmd}}, From, 
	    State = #state{connected = true}) ->
    {reply, {error, "Connection already open", ?SQL_ERROR}, State};

%% Close connection to database
handle_msg({control_cmd, {close_connection, ODBCCmd}}, From, 
	    State = #state{connected = true}) ->
    Port = State#state.port,
    NewState = State#state{connected = false, reply_to = From},
    port_command(Port,  [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, NewState};

%% If you try closing a connection that is already closed
handle_msg({control_cmd, {close_connection, ODBCCmd}}, From, State = 
	    #state{connected = false}) ->
    {reply, {error, "Connection already closed", ?SQL_ERROR}, State};

%----------------------------------------------------------------------
%% ODBC commands - require that we have a connection to the database
handle_msg({db_cmd, Cmd}, From = {Pid, _Tag},
            #state{connected = false} = State) ->
    {reply, {error, "Not connected", ?SQL_ERROR}, State};

handle_msg({db_cmd, {bind_column, ODBCCmd, ColNum, MemRef}}, From,
	   State = #state{connected = true}) ->
    Port = State#state.port,
    ColumnVec = State#state.columnvector,
    NewColumnVec = insert({MemRef,ColNum}, ColumnVec),
    NewState = State#state{columnvector = NewColumnVec, reply_to = From},
    NewODBCCmd = [ODBCCmd, integer_to_list(ColNum)],
    port_command(Port,  [NewODBCCmd, ?STR_TERMINATOR]),
    {noreply, NewState};

handle_msg({db_cmd,{close_handle, ODBCCmd}}, From, State =
	    #state{connected = true}) ->

    Port = State#state.port,
    ColumnVect    = State#state.columnvector,
    NewColumnVect = delete(ColumnVect),
    NewState = State#state{columnvector = NewColumnVect, fetchdata = false,
			  reply_to = From},
    port_command(Port,  [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, NewState};


handle_msg({db_cmd, {read_buffer, ODBCCmd, ColumnRef}}, From, 
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
	    {noreply, State#state{reply_to = From}}
    end;

handle_msg({db_cmd, {read_buffer, _ODBCCmd, _ColumnRef}}, _From, 
	    #state{fetchdata = false} = State) ->
    {reply, {error, no_data_fetched, ?SQL_ERROR}, State};

handle_msg({db_cmd, {fetch, ODBCCmd}}, From, State = 
	    #state{connected = true}) ->
    Port = State#state.port,
    port_command(Port,  [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, State#state{reply_to = From, fetchdata = true}};

%% Tag = execute | execdir | describeColumn | endTran | 
%% | numResultCols | rowCount | setConnectAttr 
handle_msg({db_cmd, {Tag, ODBCCmd}}, From, State = 
	    #state{connected = true}) ->
    Port = State#state.port,
    port_command(Port,  [ODBCCmd, ?STR_TERMINATOR]),
    {noreply, State#state{reply_to = From}};

%----------------------------------------------------------------------
%% Catch all - throws away unknown messages.
handle_msg(Request, _From, State) ->
    error_logger:error_msg("ODBC: received unexpected request: ~p~n", 
			   [Request]),
    {reply, {did_not_understand_request, Request, ?SQL_ERROR}, State}.

%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
%% Stop Call Back
handle_cast(stop, State) ->
    {stop, normal, State};

%% Send debug request to the port-program.
handle_cast({debugc, DebugCmd}, State) ->
    Port = State#state.port,
    port_command(Port,  [DebugCmd, ?STR_TERMINATOR]),
    {noreply, State};

%% Catch all - throws away unknown messages.
handle_cast(Msg, State) ->
    error_logger:error_msg("ODBC: received unexpected message: ~p~n", [Msg]),
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    error_logger:error_msg("ODBC: exit signal from port program:~p~n", 
			   [Reason]),
    {stop, {port_exit, Reason}, State};

%% If the owning process dies there is no reson to go on
handle_info({'EXIT', Pid, Reason}, State) when pid(Pid) ->
    {stop, {stopped, {'EXIT', Pid, Reason}}, State};


%----------------------------------------------------------------------
handle_info({Port, {data, BinData}}, #state{reply_to = From} = State) 
  when From =/= undefined ->
    gen_server:reply(From, BinData),
    handle_queue(State);
    
%----------------------------------------------------------------------
% Catch all - throws away unknown messages
handle_info(Info, State) ->
    error_logger:error_msg("ODBC: received unexpected info: ~p~n", [Info]),
    {noreply, State}.

%----------------------------------------------------------------------

handle_queue(State) ->
    case queue:out(State#state.queue) of
 	{empty, Queue} ->
	    {noreply, State#state{reply_to = undefined}};
 	{{value, {WaitingMsg, WaitingFrom}}, Queue}  ->
	    NewState = State#state{queue = Queue, reply_to = WaitingFrom},
	    handle_msg(WaitingMsg, WaitingFrom, NewState)
    end.


%%----------------------------------------------------------------------
%% terminate/2 and code_change/3
%%----------------------------------------------------------------------

terminate({port_exit, _Reason}, State) ->
    ok;

terminate(_Reason, State) ->
    Port = State#state.port,
    port_close(Port),
    receive
	{Port, closed} ->
	    ok;
	{'EXIT', Port, normal} ->
	    ok
    after ?DEFAULT_TIMEOUT ->
	    error_logger:error_report("Erlang ODBC-process did not receive "
				      "closed port message.")
    end.
    
code_change(_Vsn, State, _Extra) ->
    {ok, State}.

%%%======================================================================
%%% Internal functions
%%%======================================================================

%%---------------------------------------------------------------------------
%% call(Server, Msg, Timeout) -> term()
%%	Server  - pid() | Name | {global, Name} | {Name, Node} 
%%      Msg     - term()
%%      Timeout - integer() | infinity
%% Description: Sends a message to the ODBC-server and waits for
%%              the answer. It also unpacks the answer as if it is 
%%              a binary.
%%---------------------------------------------------------------------------
call(Server, Msg = {open_connection, ODBCCmd}, Timeout) ->
    do_call(Server, {control_cmd, Msg}, Timeout); 
call(Server, Msg = {close_connection, ODBCCmd}, Timeout) ->
    do_call(Server, {control_cmd, Msg}, Timeout); 
call(Server, Msg, Timeout) ->
    do_call(Server, {db_cmd, Msg}, Timeout).

do_call(Server, Msg, Timeout) ->
    Result = gen_server:call(Server, Msg, Timeout),
    if 
	binary(Result) -> % Normal case
	    binary_to_term(Result); 
	true -> % Error ocuured
	    Result
    end.

%%---------------------------------------------------------------------------
%% This is a simple table. The table contains tuples.
%% Each tuple contain a memoryreferens and a corrsponding
%% columnnumber.
%%---------------------------------------------------------------------------

insert({Ref, ColumnNr}, []) ->
    [{Ref, ColumnNr}];
insert({Ref, ColumnNr}, Tail) ->
    [{Ref, ColumnNr}| Tail].

lookup(_Ref, []) ->
    not_found;
lookup(Ref, [{Ref, ColumnNr} | _Tail]) ->
    ColumnNr;
lookup(Ref, [Head | Tail] ) ->
    lookup(Ref, Tail).

delete(_Tail) ->
    [].

%%%======================================================================
%%% Debug functions
%%%======================================================================

%%---------------------------------------------------------------------------
%% debugerl(Server, OnOff, <Level>) -> ok
%%	Server  - pid() | Name | {global, Name} | {Name, Node} 
%%	OnOff   - on | off
%%      Level   - exported | all
%% Description: Turns on tracing of messages sent and recived by
%%              the server <Server> and tracing on all, or all exported 
%%              functions, according to level <Level>, in this module.
%%              Result will be printed on stdout.
%% Note: This function is only intended for debugging and may not be used
%%       in products. Turning on this tracing will cause the program
%%       to loose performance !!!!!!!!!!!!   
%%---------------------------------------------------------------------------
debugerl(Server, OnOff) ->
    debugerl(Server, OnOff, exported).

debugerl(Server, on, exported) ->
    dbg:tracer(),
    dbg:tp(?MODULE, [{'_', [], [{return_trace}]}]),
    dbg:p(Server, [call, m]),
    ok; 

debugerl(Server, on, all) ->
    dbg:tracer(),
    dbg:tpl(?MODULE, [{'_', [], [{return_trace}]}]),
    dbg:p(Server, [call, m]),
    ok;

debugerl(Server, off, _Level) ->
    dbg:stop(),
    ok.

%%---------------------------------------------------------------------------
%% debugc(Server, OnOff) -> _
%%	Server  - pid() | Name | {global, Name} | {Name, Node} 
%%	OnOff   - on | off	
%% Description: Turns on/off the c-programs (port programs) debug-printouts.
%% Note: This function is only intended for debugging and may not be used
%%       in products. Turning on these printouts will cause the program
%%       to loose performance !!!!!!!!!!!!
%%---------------------------------------------------------------------------
debugc(Server, on) ->
    CCmd = [?DEBUG, ?DEBUG_ON, ?STR_TERMINATOR],
    gen_server:cast(Server, {debugc, CCmd});
    
debugc(Server, off) ->
    CCmd = [?DEBUG, ?DEBUG_OFF, ?STR_TERMINATOR],
    gen_server:cast(Server, {debugc, CCmd}).
    

%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%%---------------------------------------------------------------------------
%%
%% !!!!!!!!!!!!!! Only depricated functions below !!!!!!!!!!!!!!!!!!!!!!!!!!!!
%%---------------------------------------------------------------------------

%% This function is deprecated and does nothing.
%% It is included only for compatibility reason.
%%--------------------------------------------------------------------------- 
sql_alloc_handle(Server, HandleType, RefInputHandle) ->
    sql_alloc_handle(Server, HandleType, RefInputHandle, ?DEFAULT_TIMEOUT).

sql_alloc_handle(_Server, _HandleType, _RefInputHandle, _Timeout) ->
    {0,void}. 

%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% This function is deprecated and replaced by the function 
%% sqlBindColumn/[3,4].
%%---------------------------------------------------------------------------
sql_bind_col(Server, RefStmtHandle, ColNum, RefBuf) ->
    sql_bind_col(Server, RefStmtHandle, ColNum, RefBuf, ?DEFAULT_TIMEOUT).

sql_bind_col(Server, _RefStmtHandle, ColNum, RefBuf, Timeout) ->
    Ret = sqlBindColumn(Server, ColNum, RefBuf, Timeout),
    case Ret of
	{error, _ErrMsg, ErrCode} ->
	    ErrCode;
	Other ->
	    Other
    end.

%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% This function is deprecated and replaced by the function
%% sqlCloseHandle/[1,2].
%%---------------------------------------------------------------------------
sqlCloseCursor(Server) ->
    sqlCloseCursor(Server, ?DEFAULT_TIMEOUT).

sqlCloseCursor(Server, Timeout) ->
    Ret = sqlCloseHandle(Server, Timeout),
    case Ret of
	{error, _ErrMsg, ErrCode} ->
	    ErrCode;
	Other ->
	    Other
    end.

%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% This function is deprecated.
%% It is included only for compatibility reason.
%%---------------------------------------------------------------------------
sql_close_cursor(Server, RefStmtHandle) ->
    sql_close_cursor(Server, RefStmtHandle, ?DEFAULT_TIMEOUT).

sql_close_cursor(Server, _RefStmtHandle, Timeout) ->
    0.

%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% This function is deprecated and replaced by the function
%% sqlConnect/[4,5].
%%---------------------------------------------------------------------------
sql_connect(Server, RefConnHandle, DSN, UID, Auth) ->
    sql_connect(Server, RefConnHandle, DSN, UID, Auth, ?DEFAULT_TIMEOUT).

sql_connect(Server, _RefConnHandle, DSN, UID, Auth, Timeout) ->
    Ret = sqlConnect(Server, DSN, UID, Auth, Timeout),
    case Ret of
	{error, _ErrMsg, ErrCode} ->
	    ErrCode;
	Other ->
	    Other
    end.

%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% This function is deprecated and repleced by the function
%% sqlDescribeCol/[2,3].
%%---------------------------------------------------------------------------
sql_describe_col(Server, RefStmtHandle, ColNum, BufLenColName) ->
    sql_describe_col(Server, RefStmtHandle, ColNum, BufLenColName, 
		     ?DEFAULT_TIMEOUT).

sql_describe_col(Server, _RefStmtHandle, ColNum, _BufLenColName, Timeout) ->
    Ret = sqlDescribeCol(Server, ColNum, Timeout),
    case Ret of
	{error, _ErrMsg, ErrCode} ->
	    ErrCode;
	{ReturnCode, ColName, CNullable} ->
	    {ReturnCode, {ColName, -1}, -1, -1, -1, CNullable};
	Other ->
	    Other
    end.

%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% This function is deprecated and replaced by the function
%% sqlDisConnect/[1,2].
%%---------------------------------------------------------------------------
sql_disconnect(Server, RefConnHandle) ->
    sql_disconnect(Server, RefConnHandle, ?DEFAULT_TIMEOUT).

sql_disconnect(Server, _RefConnHandle, Timeout) ->
    Ret = sqlDisConnect(Server, Timeout),
    case Ret of
	{error, _ErrMsg, ErrCode} ->
	    ErrCode;
	Other ->
	    Other
    end.

%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% This function is deprecated and does nothing.
%% It is included only for compatibility reason.
%%--------------------------------------------------------------------------
sql_driver_connect(Server, RefConnHandle, InConnStr,
		   BufLenOutConnStr, DrvCompletion) ->
    sql_driver_connect(Server, RefConnHandle, InConnStr,
		       BufLenOutConnStr, DrvCompletion, ?DEFAULT_TIMEOUT).

sql_driver_connect(_Server, _RefConnHandle, _InConnStr,
		   _BufLenOutConnStr, _DrvCompletion, _Timeout) ->
    {0, void}.

%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% This function is deprecated and replaced by the function 
%% sqlEndTran/[2,3]
%%---------------------------------------------------------------------------
sql_end_tran(Server, HandleType, RefHandle, ComplType) ->
    sql_end_tran(Server, HandleType, RefHandle, ComplType, ?DEFAULT_TIMEOUT).

sql_end_tran(Server, _HandleType, _RefHandle, ComplType, Timeout) ->
    Ret = sqlEndTran(Server, ComplType, Timeout),
    case Ret of
	{error, _ErrMsg, ErrCode} ->
	    ErrCode;
	Other ->
	    Other
    end.

%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% This function is deprecated and replaced by the function
%% sqlExecDirect/[2,3].
%%---------------------------------------------------------------------------
sql_exec_direct(Server, RefStmtHandle, Stmt) ->
    sql_exec_direct(Server, RefStmtHandle, Stmt, ?DEFAULT_TIMEOUT).


sql_exec_direct(Server, _RefStmtHandle, Stmt, Timeout) ->
    Ret = sqlExecDirect(Server, Stmt, Timeout),
    case Ret of
	{error, _ErrMsg, ErrCode} ->
	    ErrCode;
	Other ->
	    Other
    end.
%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% This function is deprecated and replaced by the function
%% sqlFetch/[1,2].
%%---------------------------------------------------------------------------
sql_fetch(Server, RefStmtHandle) ->
    sql_fetch(Server, RefStmtHandle, ?DEFAULT_TIMEOUT).

sql_fetch(Server, _RefStmtHandle, Timeout) ->
    Ret = sqlFetch(Server, Timeout),
    case Ret of
	{error, _ErrMsg, ErrCode} ->
	    ErrCode;
	Other ->
	    Other
    end.

%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% This function is deprecated and does nothing.
%% It is included only for compatibility reason.
%%---------------------------------------------------------------------------
sql_free_handle(Server, HandleType, RefHandle) ->
    sql_free_handle(Server, HandleType, RefHandle, ?DEFAULT_TIMEOUT).

sql_free_handle(_Server, _HandleType, _RefHandle, _Timeout) ->
    0.
%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% This function is deprecated and does nothing
%% It is included only for compatibility reason.
%%---------------------------------------------------------------------------
sql_get_connect_attr(Server, RefConnHandle, Attr, BufType) ->
    sql_get_connect_attr(Server, RefConnHandle, Attr, BufType, 
			 ?DEFAULT_TIMEOUT).

sql_get_connect_attr(_Server, _RefConnHandle, _Attr, _BufType, _Timeout) ->
    {0, -1}.

%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% This function is deprecated and does nothing.
%% It is included only for compatibility reason.
%%---------------------------------------------------------------------------
sql_get_diag_rec(Server, HandleType, RefHandle, RecNum, BufLenErrMsg) ->
    sql_get_diag_rec(Server, HandleType, RefHandle, RecNum, BufLenErrMsg,
		     ?DEFAULT_TIMEOUT).

sql_get_diag_rec(_Server, _HandleType, _RefHandle, _RecNum, 
		 _BufLenErrMsg, _Timeout) ->
   0.

%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% This function is deprecated and replaced by the function
%% sqlNumResultCols/[1,2].
%%---------------------------------------------------------------------------
sql_num_result_cols(Server, RefStmtHandle) ->
    sql_num_result_cols(Server, RefStmtHandle, ?DEFAULT_TIMEOUT).

sql_num_result_cols(Server, _RefStmtHandle, Timeout) ->
    Ret = sqlNumResultCols(Server, Timeout),
    case Ret of
	{error, _ErrMsg, ErrCode} ->
	    ErrCode;
	Other ->
	    Other
    end.    

%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% This function is deprecated and replaced by the function
%% sql_rowCount/[1,2].
%%---------------------------------------------------------------------------
sql_row_count(Server, RefStmtHandle) ->
    sql_row_count(Server, RefStmtHandle, ?DEFAULT_TIMEOUT).

sql_row_count(Server, _RefStmtHandle, Timeout) ->
    Ret = sqlRowCount(Server, Timeout),
    case Ret of
	{error, _ErrMsg, ErrCode} ->
	    ErrCode;
	Other ->
	    Other
    end.

%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% This function is deprecated and replaced by the function.
%% sqlSetConnectAttr/[4, 5].
%%---------------------------------------------------------------------------
sql_set_connect_attr(Server, RefConnHandle, Attr, Value, BufType) ->
    sql_set_connect_attr(Server, RefConnHandle, Attr, Value, BufType,
			 ?DEFAULT_TIMEOUT).

sql_set_connect_attr(Server, _RefConnHandle, Attr, Value, _BufType, Timeout) ->
    Ret = sqlSetConnectAttr(Server,Attr,Value,Timeout),
    case Ret of
	{error, _ErrMsg, ErrCode} ->
	    ErrCode;
	Other ->
	    Other
    end.

%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% This function is deprecated and does nothing.
%% It is included only for compatibility reason.
%%---------------------------------------------------------------------------
sql_set_env_attr(Server, RefEnvHandle, Attr, Value, BufType) ->
    sql_set_env_attr(Server, RefEnvHandle, Attr, Value, BufType, 
		     ?DEFAULT_TIMEOUT).

sql_set_env_attr(_Server, _RefEnvHandle, _Attr, _Value, _BufType, 
		 _Timeout) ->
    0.

%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% This function is deprecated and replaced by the function
%% columnRef/0. 
%%---------------------------------------------------------------------------
alloc_buffer(Server, BufCType, Size) ->
    alloc_buffer(Server, BufCType, Size, ?DEFAULT_TIMEOUT).

alloc_buffer(_Server, _BufCType, _Size, _Timeout) ->
    columnRef().

%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% This function is deprecated and does nothing.
%% It is included only for compatibility reason.
%%---------------------------------------------------------------------------
dealloc_buffer(Server, RefBuf) ->
    dealloc_buffer(Server, RefBuf, ?DEFAULT_TIMEOUT).

dealloc_buffer(_Server, _RefBuf, _Timeout) ->
    ok.
%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% This function is deprecated and replaced by the function 
%% readData/[2,3].
%%---------------------------------------------------------------------------
read_buffer(Server, RefBuf) ->
    read_buffer(Server, RefBuf, ?DEFAULT_TIMEOUT).

read_buffer(Server, RefBuf, Timeout) ->
    Ret = readData(Server, RefBuf, Timeout),
    case Ret of
	{ok, Value} ->
	    {ok, {Value, -1}}; 
	Other ->
	    Other
    end.

%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% This function is deprecated and does nothing.
%% It is included only for compatibility reason.
%%---------------------------------------------------------------------------
init_env(Server) ->
    init_env(Server, ?DEFAULT_TIMEOUT).

init_env(_Server, _Timeout) ->
    {ok, void}.

%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% This function is deprecated and replaced by the function 
%% erl_connect/[2,3,4,5].
%%---------------------------------------------------------------------------
connect(Server, RefEnvHandle, ConnectStr) ->
    connect(Server, RefEnvHandle, ConnectStr, ?DEFAULT_TIMEOUT).

connect(Server, _RefEnvHandle, ConnectStr, Timeout) ->
    Ret = erl_connect(Server, ConnectStr, Timeout),
    case Ret of
	ok ->
	    {ok, void};
	{error,ErrMsg, ErrCode} ->
	    {error, {connect, [{void, {ErrCode, ErrMsg, -1}}]}};
	Other ->
	    Other
    end.

connect(Server, RefEnvHandle, DSN, UID, PWD) ->
    connect(Server, RefEnvHandle, DSN, UID, PWD, ?DEFAULT_TIMEOUT).

connect(Server, _RefEnvHandle, DSN, UID, PWD, Timeout) ->
    Ret = erl_connect(Server, DSN, UID, PWD, Timeout),
    case Ret of
	ok ->
	    {ok, void};
	{error,ErrMsg, ErrCode} ->
	    {error, {connect, [{void, {ErrCode, ErrMsg, -1}}]}};
	Other ->
	    Other
    end.
%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% This function is deprecated and replaced by the function
%% erl_executeStmt/[2,3].
%%---------------------------------------------------------------------------
execute_stmt(Server, RefConnHandle, Stmt) ->
    execute_stmt(Server, RefConnHandle, Stmt, ?DEFAULT_TIMEOUT).

execute_stmt(Server, _RefConnHandle, Stmt, Timeout) ->
    Ret = erl_executeStmt(Server, Stmt, Timeout),
    case Ret of
	{error,_ErrMsg1, ErrMsg2} ->
	    {error,{execute_stmt, [{void, {-1, ErrMsg2, -1}}]}};
	{selected, ColumnList, RowData} ->
	    NewColumnList = ColumnList, % For new ColumnList 
	    NewRowData = bar(RowData),
	    {selected,NewColumnList, NewRowData};
	Other ->
	    Other
    end.
    
%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% Bar convert a list in format [[El11, El12, ...], [El21, ...],....] 
%% to same lists but each element (El11,..) converted to a string. 
bar([])->
    [];
bar([[X|Xs]|Ys])->
    [lists:append(bar2(X),bar1(Xs))|bar(Ys)].
bar1([X])->
    bar2(X);
bar1([X|Xs]) ->
    lists:append(bar2(X),bar1(Xs)).
bar2([])->
    [" "];
bar2(X) when list(X) ->
    io_lib:format("~s",[X]);
bar2(X) when float(X) ->
    io_lib:format("~f",[X]);
bar2(X) ->
    io_lib:format("~p",[X]).

%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% This function is deprecated and replaced by the function
%% erl_disconnect/[1,2].
%%---------------------------------------------------------------------------
disconnect(Server, RefConnHandle) ->
    disconnect(Server, RefConnHandle, ?DEFAULT_TIMEOUT).

disconnect(Server, _RefConnHandle, Timeout) ->
    Ret = erl_disconnect(Server, Timeout),
    case Ret of
	ok ->
	    ok;
	{error, ErrMsg, ErrCode} ->
	    {error, {disconnect, [{void, {ErrCode, ErrMsg, -1}}]}};
	Other ->
	    Other
    end.

%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% This function is deprecated and does nothing.
%% It is included only for compatibility reason.
%%-------------------------------------------------------------------
terminate_env(Server, RefEnvHandle) ->
    terminate_env(Server, RefEnvHandle, ?DEFAULT_TIMEOUT).

terminate_env(_Server, _RefEnvHandle, _Timeout) ->
    ok.
%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
%% This function is deprecated and does nothing.
%% It is included only for compatibility reason.
%%-------------------------------------------------------------------
display_size(SqlType, ColumnSize) when 
  integer(SqlType), integer(ColumnSize) ->
    -1.
%%%&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


