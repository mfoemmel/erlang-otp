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

%% Note that the SQL syntax is database and ODBC Driver dependent.
%% Error handling is not covered by the example.

start() ->
    % Start a new ODBC server. The application must already be started.
    {ok, _Pid} = odbc:start_link({local, odbc1}, [], []),



    % Connect to the database (also loads the Driver).
    ?SQL_SUCCESS =
      odbc:sqlConnect(odbc1, ?DSN, ?UID, ?PWD, infinity),

    % Create a new table.
    CreateStmt = 
	"CREATE TABLE TAB1 (ID number(3), DATA char(10))",
    ?SQL_SUCCESS = odbc:sqlExecDirect(odbc1, CreateStmt, infinity),

    % Print how many rows were affected by the statement.
    {?SQL_SUCCESS, NAffectedRows1} = 
	odbc:sqlRowCount(odbc1, infinity),
    io:format("Create: Number of affected rows: ~p~n", [NAffectedRows1]),


    % Insert a new row.
    InsertStmt1 = "INSERT INTO TAB1 VALUES (1, 'a1a2a3a4a5')",
    ?SQL_SUCCESS = odbc:sqlExecDirect(odbc1,InsertStmt1, infinity),

    % Print how many rows were affected by the statement.
    {?SQL_SUCCESS, NAffectedRows2} =
      odbc:sqlRowCount(odbc1, infinity),
    io:format("Insert: Number of affected rows: ~p~n", [NAffectedRows2]),

    % Select all columns from all rows.
    SelectStmt = "SELECT * FROM TAB1",
    ?SQL_SUCCESS = odbc:sqlExecDirect(odbc1, SelectStmt, infinity),

    % Print how many columns there are in the table resulting from the 
    % statement.
    {?SQL_SUCCESS, NSelectedCols} =
      odbc:sqlNumResultCols(odbc1, infinity),
    io:format("Select: Number of columns: ~p~n", [NSelectedCols]),

    % Describe the column(s) of the resulting table.
    {?SQL_SUCCESS, ColName1, Nullable1} =
      odbc:sqlDescribeCol(odbc1, 1, infinity),
    {?SQL_SUCCESS, ColName2, Nullable2} =
      odbc:sqlDescribeCol(odbc1, 2, infinity),

    % Create references for columns
    Buf1 = odbc:columnRef(),
    Buf2 = odbc:columnRef(),

    % Bind the refererces to the columns.
    ?SQL_SUCCESS = odbc:sqlBindColumn(odbc1, 1, Buf1, infinity),
    ?SQL_SUCCESS = odbc:sqlBindColumn(odbc1, 2, Buf2, infinity),

    % Fetch the first row of selected rows.
    ?SQL_SUCCESS = odbc:sqlFetch(odbc1, infinity),

    % Read the value from the buffer(s).
    {ok, ColValue1} =
      odbc:readData(odbc1, Buf1, infinity),
    io:format("Select: Column name: ~p, Data: ~p~n", [ColName1, ColValue1]),
    {ok, ColValue2} =
      odbc:readData(odbc1, Buf2, infinity),
    io:format("Select: Column name: ~p, Data: ~p~n", [ColName2, ColValue2]),

    % Check that there are no more rows to fetch.
    ?SQL_NO_DATA = odbc:sqlFetch(odbc1, infinity),

    % Close the cursor on the statement.
    ?SQL_SUCCESS = odbc:sqlCloseCursor(odbc1, infinity),

    % Delete the table.
    DropStmt = "DROP TABLE TAB1",
    ?SQL_SUCCESS = odbc:sqlExecDirect(odbc1, DropStmt, infinity),

    % Print how many rows were affected by the statement.
    {?SQL_SUCCESS, NAffectedRows3} = 
	odbc:sqlRowCount(odbc1, infinity),
    io:format("Delete: Number of affected rows: ~p~n", [NAffectedRows3]),

    % Disconnect from the database.
    ?SQL_SUCCESS = odbc:sqlDisConnect(odbc1, infinity),

    % Stop the server.
    ok = odbc:stop(odbc1).









