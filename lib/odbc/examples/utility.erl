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

start() ->
    % Start a new ODBC server. The application must already be started.
    {ok, Pid1} = odbc:start_link([], []),
					
    % Connect to the database.
    ok  = 
	odbc:erl_connect(Pid1, ?ConnectStr, infinity),

    % Create a table
    % By default, all transactions are automatically committed.
    CreateStmt = "CREATE TABLE testtable (ID number(3), DATA char(100))",
    SqlRet1 =
	odbc:erl_executeStmt(Pid1, CreateStmt, infinity),
    io:format("executeStmt Create a table returns ~p~n",[SqlRet1]),
   
    % Insert data into the table
    InsertStmt = "INSERT INTO testtable VALUES(1, '1a2b3c4d5e')",
    SqlRet2 = 
	odbc:erl_executeStmt(Pid1, InsertStmt, infinity),
    io:format("executeStmt Insert into table returns ~p~n",[SqlRet2]),

    % Select all rows
    % By default, all transactions are automatically committed.
    SelectStmt = "SELECT * FROM testtable",
    {selected, Columnnames, Rows} =
	odbc:erl_executeStmt(Pid1, SelectStmt, infinity),
    io:format("execute_stmt Select statement returns ~p~n",
	      [{selected, Columnnames, Rows}]),
    
    % Delete the table
    DropStmt = "DROP TABLE testtable",
   {updated, NAffectedRows3} =
	odbc:erl_executeStmt(Pid1, DropStmt, infinity),
    io:format("Delete: Number of affected rows: ~p~n", [NAffectedRows3]),
    
    % Disconnect.
    odbc:erl_disconnect(Pid1, infinity),

    % Stop the server.
    odbc:stop(Pid1, infinity).








