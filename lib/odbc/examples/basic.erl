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
-module(basic).

-export([start/0]).


% Contains macros defined by the ODBC standard.
-include("/clearcase/otp/libraries/odbc/include/odbc.hrl").


% These strings depend on how your ODBC Driver is configured.
% You need to fill in your own values.
%-define(DSN, "Oracle8").
%-define(UID, "myself").
%-define(PWD, "secret").


% The maximum length for column names + 1.
% The 1 is there to allow room for a null-termination character.
-define(BufLenColName, 64).

%% The maximum length of table data + 1.
-define(MaxDataBufLen, 1024).

%% Note that the SQL syntax is database and ODBC Driver dependent.
%% Error handling is not covered by the example.
%%
start() ->
    % Start a new ODBC server. The application must already be started.
    {ok, _Pid} = odbc:start_link({local, odbc1}, [], []),

    % Allocate an environment handle (also loads the Driver Manager).
    {?SQL_SUCCESS, EnvHandle} =
      odbc:sql_alloc_handle(odbc1, ?SQL_HANDLE_ENV, ?SQL_NULL_HANDLE, infinity),

    % Set the ODBC version attribute to tell the Driver we're a 3.0 application.
    ?SQL_SUCCESS =
      odbc:sql_set_env_attr(odbc1,
			      EnvHandle,
			      ?SQL_ATTR_ODBC_VERSION,
			      ?SQL_OV_ODBC3,
			      ?SQL_C_ULONG,
			      infinity),

    % Allocate a connection handle.
    {?SQL_SUCCESS, ConnectionHandle} =
      odbc:sql_alloc_handle(odbc1, ?SQL_HANDLE_DBC, EnvHandle, infinity),

    % Connect to the database (also loads the Driver).
    ?SQL_SUCCESS =
      odbc:sql_connect(odbc1, ConnectionHandle, ?DSN, ?UID, ?PWD, infinity),

    % Turn the autocommit mode off (if you don't want it).
    ?SQL_SUCCESS = odbc:sql_set_connect_attr(odbc1,
					     ConnectionHandle,
					     ?SQL_ATTR_AUTOCOMMIT,
					     ?SQL_AUTOCOMMIT_OFF,
					     ?SQL_C_ULONG,
					     infinity),

    % Allocate a statement handle.
    {?SQL_SUCCESS, StmtHandle} =
      odbc:sql_alloc_handle(odbc1, ?SQL_HANDLE_STMT, ConnectionHandle, 
                            infinity),

    % Create a new table.
    CreateStmt = "CREATE TABLE TAB1 (ID number(3), DATA char(10))",
    ?SQL_SUCCESS = odbc:sql_exec_direct(odbc1, StmtHandle, CreateStmt, 
                                        infinity),

    % Print how many rows were affected by the statement.
    {?SQL_SUCCESS, NAffectedRows1} = odbc:sql_row_count(odbc1, StmtHandle, 
                                                        infinity),
    ok = io:format("Create: Number of affected rows: ~p~n", [NAffectedRows1]),

    % Commit the transaction.
    ?SQL_SUCCESS =
      odbc:sql_end_tran(odbc1,
			  ?SQL_HANDLE_DBC,
			  ConnectionHandle,
			  ?SQL_COMMIT,
			  infinity),

    % Insert a new row.
    InsertStmt = "INSERT INTO TAB1 VALUES (1, 'a1a2a3a4a5')",
    ?SQL_SUCCESS = odbc:sql_exec_direct(odbc1, StmtHandle, InsertStmt, 
                                        infinity),

    % Print how many rows were affected by the statement.
    {?SQL_SUCCESS, NAffectedRows2} =
      odbc:sql_row_count(odbc1, StmtHandle, infinity),
    ok = io:format("Insert: Number of affected rows: ~p~n", [NAffectedRows2]),

    % Commit the transaction.
    ?SQL_SUCCESS =
      odbc:sql_end_tran(odbc1,
			  ?SQL_HANDLE_DBC,
			  ConnectionHandle,
			  ?SQL_COMMIT,
			  infinity),

    % Select the DATA column from all rows.
    SelectStmt = "SELECT DATA FROM TAB1",
    ?SQL_SUCCESS = odbc:sql_exec_direct(odbc1, StmtHandle, SelectStmt, infinity),

    % Print how many columns there are in the table resulting from the 
    % statement.
    {?SQL_SUCCESS, NSelectedCols} =
      odbc:sql_num_result_cols(odbc1, StmtHandle, infinity),
    ok = io:format("Select: Number of columns: ~p~n", [NSelectedCols]),

    % Describe the column(s) of the resulting table.
    {?SQL_SUCCESS, {ColName, _LenColName}, SqlType, ColSize, _DecDigits, 
                    _Nullable} =
      odbc:sql_describe_col(odbc1, StmtHandle, 1, ?BufLenColName, infinity),

    % Calculate the size of the buffer(s) we're going to use to retrieve data.
    % Make sure you protect yourself from trying to allocate huge amounts of
    % memory.
    DispSize = odbc:display_size(SqlType, ColSize),
    BufSz =
	if
	    ColSize > ?MaxDataBufLen ->
		?MaxDataBufLen;
	    true ->
		DispSize
	end,

    % Allocate data buffer(s).
    {ok, Buf} =
      odbc:alloc_buffer(odbc1, ?SQL_C_CHAR, BufSz, infinity),

    % Bind the buffer(s) to the column.
    ?SQL_SUCCESS = odbc:sql_bind_col(odbc1, StmtHandle, 1, Buf, infinity),

    % Fetch the first row into the bound buffer(s) (only one buffer bound here).
    ?SQL_SUCCESS = odbc:sql_fetch(odbc1, StmtHandle, infinity),

    % Read the value from the buffer(s).
    {ok, {ColValue, _LenColValue}} =
      odbc:read_buffer(odbc1, Buf, infinity),
    io:format("Select: Column name: ~p, Data: ~p~n", [ColName, ColValue]),

    % Check that there are no more rows to fetch.
    ?SQL_NO_DATA = odbc:sql_fetch(odbc1, StmtHandle, infinity),

    % Close the cursor on the statement.
    ?SQL_SUCCESS = odbc:sql_close_cursor(odbc1, StmtHandle, infinity),

    % Deallocate the buffer(s).
    ok = odbc:dealloc_buffer(odbc1, Buf, infinity),

    % Delete the table.
    DropStmt = "DROP TABLE TAB1",
    ?SQL_SUCCESS = odbc:sql_exec_direct(odbc1, StmtHandle, DropStmt, infinity),

    % Print how many rows were affected by the statement.
    {?SQL_SUCCESS, NAffectedRows3} = odbc:sql_row_count(odbc1, StmtHandle, 
                                                        infinity),
    ok = io:format("Delete: Number of affected rows: ~p~n", [NAffectedRows3]),

    % Commit the transaction.
    ?SQL_SUCCESS =
      odbc:sql_end_tran(odbc1,
			  ?SQL_HANDLE_DBC,
			  ConnectionHandle,
			  ?SQL_COMMIT,
			  infinity),

    % Free the statement handle.
    ?SQL_SUCCESS =
	odbc:sql_free_handle(odbc1, ?SQL_HANDLE_STMT, StmtHandle, infinity),

    % Disconnect from the database.
    ?SQL_SUCCESS = odbc:sql_disconnect(odbc1, ConnectionHandle, infinity),

    % Free the connection handle.
    ?SQL_SUCCESS =
      odbc:sql_free_handle(odbc1, ?SQL_HANDLE_DBC, ConnectionHandle, infinity),

    % Free the environment handle.
    ?SQL_SUCCESS =
      odbc:sql_free_handle(odbc1, ?SQL_HANDLE_ENV, EnvHandle, infinity),

    % Stop the server.
    ok = odbc:stop(odbc1).


