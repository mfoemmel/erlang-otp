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

-export([init/0,stop/0]).
-export([create/0,insert/0, drop/0, select/0]).


% Contains macros defined by the ODBC standard.
-include("/home/ingmey/odbc/include/odbc.hrl").


% These strings depend on how your ODBC Driver is configured.
% You need to fill in your own values.
%-define(DSN, "Oracle8").
%-define(UID, "myself").
%-define(PWD, "secret").
-define(SERV, odbc1).


init() ->
    % Start a new ODBC server. The application must already be started.
    {ok, Pid} = odbc:start_link({local, ?SERV},[], []),

    % Connect to the database (also loads the Driver).
    Ret =
      odbc:sqlConnect(?SERV, ?DSN, ?UID, ?PWD, infinity),
    io:format("sqlConnect returns ~p~n",[Ret]).

create() ->
    % Create a new table.
    CreateStmt = 
	"CREATE TABLE TAB3 (ID int, DATA varchar(50))",
    Ret1 = odbc:sqlExecDirect(?SERV, CreateStmt, infinity),
    io:format("sqlExecDirect returns ~p~n",[Ret1]),

    Ret2 = odbc:sqlCloseHandle(?SERV, infinity),
    io:format("sqlCloseHandle returns ~p~n",[Ret2]).
    
insert() ->
    % Insert a new row.
    Str1 = "INSERT INTO TAB3 VALUES (1,'",
    Str2 = string:chars($a,50),
    InsertStmt1 = lists:flatten([Str1, Str2, "')"]),
    Ret1 = odbc:sqlExecDirect(?SERV,InsertStmt1, infinity),
    io:format("sqlExecDirect returns ~p~n",[Ret1]),

    % Print how many rows were affected by the statement.
    Ret2 = odbc:sqlCloseHandle(?SERV, infinity),
    io:format("sqlCloseHandle returns ~p~n",[Ret2]).

select() ->
    % Select all columns from all rows.
    SelectStmt = "SELECT * FROM TAB3",
    Ret1 = odbc:sqlExecDirect(?SERV, SelectStmt, infinity),
    io:format("sqlExecDirect returns ~p~n",[Ret1]),
    % Print how many columns there are in the table resulting from the 
    % statement.
    {?SQL_SUCCESS, NSelectedCols} =
      odbc:sqlNumResultCols(?SERV, infinity),
    io:format("Select: Number of columns: ~p~n", [NSelectedCols]),

    % Create references for columns
    {ok,Ref1} = odbc:columnRef(),
    {ok,Ref2} = odbc:columnRef(),
    % Bind the refererces to the columns.
    Ret6 = odbc:sqlBindColumn(?SERV, 1, Ref1, infinity),
    io:format("sqlBindColumn returns ~p~n",[Ret6]),
    Ret7 = odbc:sqlBindColumn(?SERV, 2, Ref2, infinity),
    io:format("sqlBindColumn returns ~p~n",[Ret7]),
    % Fetch the first row of selected rows.
    Ret8 = odbc:sqlFetch(?SERV, infinity),
    io:format("sqlFetch returns ~p~n",[Ret1]),
    % Read the value from the buffer(s).
    {ok, ColValue1} =
	odbc:readData(?SERV, Ref1, infinity),
    io:format("Select: Column 1 data:~p~n",[ColValue1]),
    {ok, ColValue2} =
      odbc:readData(?SERV, Ref2, infinity),
    io:format("Select: Column 2 data:~p~n",[ColValue2]),

    % Check that there are no more rows to fetch.
    ?SQL_NO_DATA = odbc:sqlFetch(?SERV, infinity),
    % Close the cursor on the statement.
    Ret12 = odbc:sqlCloseHandle(?SERV, infinity),
    io:format("sqlCloseHandle returns ~p~n",[Ret12]).

drop() ->
    % Delete the table.
    DropStmt = "DROP TABLE TAB3",
    Ret = odbc:sqlExecDirect(?SERV, DropStmt, infinity),
    io:format("sqlExecDirect returns ~p~n",[Ret]).

stop() ->
    % Disconnect from the database.
    Ret = odbc:sqlDisConnect(?SERV, infinity),
    % Stop the server.
    odbc:stop(?SERV).
