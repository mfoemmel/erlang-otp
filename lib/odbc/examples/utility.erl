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
-module(utility).

-export([start/0]).


% This string depends on how your ODBC Driver is configured.
% You need to fill in your own value.
%-define(ConnectStr, "DSN=Oracle8;UID=myself;PWD=secret").


%% Note that the SQL syntax is database and ODBC Driver dependent.
%%
start() ->
    % Start a new ODBC server. The application must already be started.
    {ok, _Pid} = odbc:start_link({local, odbc1}, [], []),

    % Initialise the environment (also loads the Driver Manager).
    {ok, EnvHandle} = odbc:init_env(odbc1, infinity),

    % Load the Driver and connect to the database.
    {ok, ConnectionHandle} = odbc:connect(odbc1, EnvHandle, ?ConnectStr, infinity),

    % Create a new table.
    % By default, all transactions are automatically committed.
    CreateStmt = "CREATE TABLE TAB1 (ID number(3), DATA char(10))",
    {updated, NAffectedRows1} =
	odbc:execute_stmt(odbc1, ConnectionHandle, CreateStmt, infinity),
    ok = io:format("Create: Number of affected rows: ~p~n", [NAffectedRows1]),

    % Insert a row.
    InsertStmt = "INSERT INTO TAB1 VALUES (1, 'a1a2a3a4a5')",
    {updated, NAffectedRows2} =
	odbc:execute_stmt(odbc1, ConnectionHandle, InsertStmt, infinity),
    ok = io:format("Insert: Number of affected rows: ~p~n", [NAffectedRows2]),

    % Select all rows.
    SelectStmt = "SELECT * FROM TAB1",
    {selected, ColumnNames, Rows} =
	odbc:execute_stmt(odbc1, ConnectionHandle, SelectStmt, infinity),
    ok = io:format("Select: Column names: ~p, Rows: ~p~n", [ColumnNames, Rows]),

    % Delete the table.
    DropStmt = "DROP TABLE TAB1",
    {updated, NAffectedRows3} =
	odbc:execute_stmt(odbc1, ConnectionHandle, DropStmt, infinity),
    ok = io:format("Delete: Number of affected rows: ~p~n", [NAffectedRows3]),

    % Disconnect.
    ok = odbc:disconnect(odbc1, ConnectionHandle, infinity),

    % Terminate the environment.
    ok = odbc:terminate_env(odbc1, EnvHandle, infinity),

    % Stop the server.
    ok = odbc:stop(odbc1, infinity).
