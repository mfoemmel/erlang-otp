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

-module(odbc).

-behaviour(gen_server).


%% External exports------------------------------------
%% Basic API
-export([sqlBindColumn/3, sqlBindColumn/4]).
-export([sqlCloseCursor/1, sqlCloseCursor/2]).
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
-export([erl_disconnect/2, erl_disconnect/3]).

%% Start/stop
-export([start_link/2, start_link/3]).
-export([stop/1, stop/2]).

%% Miscellaneous
-export([display_size/2]).
%%------------------------------------------------------

%% Internal exports (for debug)
-export([get_state/1]).


%% Internal exports (for supervisor).
-export([start_link_sup/2, start_link_sup/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).


%% Internal state
-record(state, {port,                        % The port to the c-node.
		connected = false,           % Connected to database. 
		fetchdata = false,           % fetched data before read
		clientPid,                   % Pid of the odbc-client.
		columnvector = []            % memoryref for columns
	       }).

%%------------------------------------------------------
%% Internal macros
%% Path to C server main
-define(ServerDir, filename:nativename(
		     filename:join(code:priv_dir(odbc), "bin"))).

%% Name of C server main
-define(ServerProg, "odbcserver").

%% Default timeout.
-define(DefaultTimeout, 5000).


%%%----------------------------------------------
%% DEBUG MACRO

-ifdef (DEBUG).
-define(DBG(Format, Args), io:format("Debug Module:~p line:~p: "
				     ++Format++ "~n",
                                     [?MODULE,?LINE]++Args)).
-else.
-define(DBG(F,A),[]).
-endif.


%% External exports------------------------------------
%% These functions are deprecated and is included only for 
%% compatibility reason. Some of them do nothing and some 
%% are replaced by function from above.
%% Deprecated Basic functions
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

%% Deprecated Utility functions
-export([init_env/1, init_env/2]).
-export([connect/3, connect/4, connect/5, connect/6]).
-export([execute_stmt/3, execute_stmt/4]).
-export([disconnect/2, disconnect/3]).
-export([terminate_env/2, terminate_env/3]).

%%%----------------------------------------------------------
%%% Start and Stop
%%%----------------------------------------------------------


%%---------------------------------------------------
%% start_link/[2, 3]
%%
%% Starts the ODBCE server and the C node.
%% Links to the server to the calling process.
%% Registers the new server with the supervisor.
%%
%% start_link(ServerName, Args, Options, Client) ->
%% start_link(Args, Options, Client) ->
%%    Result
%% Args -> []                      ; Not used.
%% Options -> [Opt]                ; List of options.
%% Opt -> {timeout, integer()} |   ; Timeout for initialisation of gen_server.
%%        {debug, [Dbg]}           ;Debug options, see gen_server.
%% ServerName -> {local, atom()} | ; When supplied, causes the server to be
%%               {global, atom()}    registered, locally or globally. If the
%%                                   server is started without a name it can
%%                                   only be called using the returned Pid.
%% Result -> {ok, pid()} |         ; The pid of the erl. server.
%%           {error, Reason}         Error tuple.
%% Reason -> {already_started, pid()}| ; Server already started.
%%           timeout |               Timeout expired.
%%           {no_odbcserver, Info}   Can't start odbcserver. The C-program 
%                                    may not
%%                                   have been found or may not have been
%%                                   executable e.g.
%% Info -> string()                ; More information.
%%


start_link(Args, Options) ->
    supervisor:start_child(odbc_sup, [[{client, self()} | Args], Options]).

start_link(ServerName, Args, Options) ->
    supervisor:start_child(odbc_sup,
			   [ServerName, [{client, self()} | Args], Options]).



%% start_link_sup/[2, 3]
%%
%% Called by the supervisor to start a new server instance.
%% (start_link calls the supervisor which calls start_link_sup.)
%%
start_link_sup(Args, Options) ->
    gen_server:start_link(?MODULE, Args, Options).

start_link_sup(ServerName, Args, Options) ->
    gen_server:start_link(ServerName, ?MODULE, Args, Options).



%%-------------------------------------------------------
%% stop/[1, 2]
%%
%% Stops the ODBCE server and the C program.
%%
%% stop(Server) ->
%% stop(Server, Timeout) ->
%%    ok
%%
%% Server -> pid() |                 ; The pid of the server,
%%           Name |                    a registered name,
%%           {global, Name} |          a globally registered name, or
%%           {Name, Node}              a registered name on a remote node.
%% Timeout -> integer() |            ; Max time (ms) for serving the request.
%%            infinity
%%

stop(Server) ->
    stop(Server, ?DefaultTimeout).

stop(Server, Timeout) ->
    gen_server:call(Server, stop, Timeout).



%%%--------------------------------------------------------------------------
%%% Basic API
%%%--------------------------------------------------------------------------

%% sqlBindColumn/[3, 4]
%% Assigns a referens to a column
%% (binds a referens to a  column).
%%
%% sqlBindColumn(Server, ColNum, Ref) ->
%% sqlBindColumn(Server, ColNum, Ref, Timeout) ->
%%   Result | {error, ErrMsg, ErrCode}
%%
%% Server -> pid() |                  ; The pid of the server,
%%           Name |                     a registered name,
%%           {global, Name} |           a globally registered name, or
%%           {Name, Node}               a registered name on a remote node.
%% ColNum -> integer()                ; Column number from left to right
%%                                      starting at 1.
%% Ref -> term()                      ; Reference to the column with ColNum.
%% Timeout -> integer() |             ; timeout (ms) for genserver
%%            infinity
%% Result -> ?SQL_SUCCESS |
%%           ?SQL_SUCCESS_WITH_INFO 
%% ErrMsg -> string()                 ; The error message.
%% ErrCode -> ?SQL_INVALID_HANDLE |
%%            ?SQL_EROR
%%

sqlBindColumn(Server, ColNum, Ref) ->
    sqlBindColumn(Server, ColNum, Ref, ?DefaultTimeout).

sqlBindColumn(Server, ColNum, Ref, Timeout) ->
    chk_int(ColNum, sqlBindColumn, 2),
    CmdStr = "bincol",
    Ret = gen_server:call(Server,{bindcolumn, CmdStr, ColNum, Ref},
			  Timeout),
    ?DBG("sqlBindColumn gen_server return: ~p",[Ret]),
    case Ret of
	{ok, RetMsg, RetCode} ->
	    RetCode;
	{error, ErrMsg, ErrCode} ->
	    {error, ErrMsg, ErrCode};
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	Other ->
	    Other
    end.


%% sqlCloseCursor/[1, 2]
%% Closes a cursor that has been opened on a statement and discards pending 
%% results.
%%
%% sqlCloseCursor(Server) ->
%% sqlCloseCursor(Server, Timeout) ->
%%   Result | {error, ErrMsg, ErrCode}
%%
%% Server -> pid() |                    ; The pid of the server,
%%           Name |                       a registered name,
%%           {global, Name} |             a globally registered name, or
%%           {Name, Node}                 a registered name on a remote node.
%% Timeout -> integer() |               ; timeout (ms) for genserver.
%%            infinity
%% Result -> ?SQL_SUCCESS |
%%           ?SQL_SUCCESS_WITH_INFO
%% ErrMsg -> string()                   ; Error message
%% ErrCode -> ?SQL_INVALID_HANDLE |
%%            ?SQL_ERROR

sqlCloseCursor(Server) ->
    sqlCloseCursor(Server, ?DefaultTimeout).

sqlCloseCursor(Server, Timeout) ->
    CmdStr = "closec",
    Ret = gen_server:call(Server,{closecursor, CmdStr},Timeout),
    ?DBG("sqlCloseCursor gen_server call returns: ~p",[Ret]),
    case Ret of
	% Succeeded in making ODBC call (which may have returned an error)
	{error, ErrMsg, ErrCode} ->
	    {error, ErrMsg, ErrCode};
	{ok, Result, ReturnCode} ->
	    ReturnCode;
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	Other ->
	    Other
    end.


%% sqlConnect/[4, 5]
%% Establishes a connection to a driver and a data source.
%%
%% sqlConnect(Server, DSN, UID, Auth) ->
%% sqlConnect(Server, DSN, UID, Auth, Timeout) ->
%%   Result | {error, ErrMsg, ErrCode}
%%
%% Server -> pid() |                  ; The pid of the server,
%%           Name |                     a registered name,
%%           {global, Name} |           a globally registered name, or
%%           {Name, Node}               a registered name on a remote node.
%% DSN -> string()                    ; The name of the data source.
%% UID -> string()                    ; The user ID.
%% Auth -> string()                   ; The user's password.
%% Timeout -> integer() |             ; timeout (ms) for genserver.
%%            infinity
%% Result -> ?SQL_SUCCESS | 
%%           ?SQL_SUCCESS_WITH_INFO 
%% ErrMsg -> string()                 ; The error message
%% ErrCode -> ?SQL_INVALID_HANDLE |
%%            ?SQL_ERROR

sqlConnect(Server, DSN, UID, Auth) ->
    sqlConnect(Server, DSN, UID, Auth, ?DefaultTimeout).

sqlConnect(Server, DSN, UID, Auth, Timeout) ->
    chk_str(DSN, sqlConnect, 2),
    chk_str(UID, sqlConnect, 3),
    chk_str(Auth, sqlConnect, 4),

%% When there are not any user passsword
    N = string:len(Auth),
    ConnectStr = case N of
	0 -> 
	    "opendbDSN=" ++ DSN ++ ";UID=" ++ UID;
	NotZero-> 
	    "opendbDSN=" ++ DSN ++ ";UID=" ++ UID ++ ";PWD="++ Auth
    end,
    Ret = gen_server:call(Server,{openconnection, ConnectStr, self()},
			  Timeout),
    ?DBG("sqlConnect gen_server call returns: ~p",[Ret]),
    case Ret of
	% Succeeded in making ODBC call (which may have returned an error)
	{error, ErrMsg, ErrCode} ->
	    {error, ErrMsg, ErrCode};
	{ok, Result, ReturnCode} ->
	    ReturnCode;
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	Other ->
	    Other
    end.


%% sqlDescribeCol/[2, 3]
%% Returns the result descriptor -- column name, type, column size,
%% decimal digits, and nullability -- for one column in the result set.
%%
%% sqlDescribeCol(Server, ColNum) ->
%% sqlDescribeCol(Server, ColNum, Timeout) ->
%%   {Result, ColName, Nullable} | {error, ErrMsg, ErrCode}
%%
%% Server -> pid() |                  ; The pid of the server,
%%           Name |                     a registered name,
%%           {global, Name} |           a globally registered name, or
%%           {Name, Node}               a registered name on a remote node.
%% ColNum -> integer()                ; Column number from left to right
%%                                      starting at 1.    
%% Timeout -> integer() |             ; timeout (ms) for genserver
%%            infinity
%% Result -> ?SQL_SUCCESS |
%%           ?SQL_SUCCESS_WITH_INFO |
%% ColName -> string()                ; The column name.
%% Nullable -> ?SQL_NO_NULLS |        ; Indicates whether the column allows
%%             ?SQL_NULLABLE |          NULL values or not.
%%             ?SQL_NULLABLE_UNKNOWN
%% ErrMsg -> string()                 ; The error message
%% ErrCode -> ?SQL_INVALID_HANDLE |
%%            ?SQL_ERROR

sqlDescribeCol(Server, ColNum) ->
    sqlDescribeCol(Server, ColNum, ?DefaultTimeout).

sqlDescribeCol(Server, ColNum, Timeout) ->
    chk_int(ColNum, sqlDescribeCol, 2),
    CmdStr = "descol" ++ integer_to_list(ColNum),
    Ret = gen_server:call(Server,{describeColumn, CmdStr},Timeout),
    ?DBG("sqlDescribeCol gen_server call returns: ~p",[Ret]),
    case Ret of
	% Succeeded in making ODBC call (which may have returned an error)
	{error, ErrMsg, ErrCode} ->
	    {error, ErrMsg, ErrCode};
	{ok, ReturnCode, CName, CNullable} ->
	    {ReturnCode, CName, CNullable};
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	Other ->
	    Other
    end.

%% sqlDisConnect/[1, 2]
%% Close the connection.
%%
%% sqlDisConnect(Server) ->
%% sqlDisconnect(Server, Timeout) ->
%%   Result | {error, ErrMsg, ErrCode}
%%
%% Server -> pid() |                    ; The pid of the server,
%%           Name |                       a registered name,
%%           {global, Name} |             a globally registered name, or
%%           {Name, Node}                 a registered name on a remote node.
%% Timeout -> integer() |               ; timeout (ms) for genserver.
%%            infinity
%% Result -> ?SQL_SUCCESS | 
%%           ?SQL_SUCCESS_WITH_INFO
%% ErrMsg -> string()                   ; Error message.
%% ErrCode -> ?SQL_INVALID_HANDLE |
%%            ?SQL_ERROR

sqlDisConnect(Server) ->
    sqlDisConnect(Server, ?DefaultTimeout).

sqlDisConnect(Server, Timeout) ->
    CloseStr = "closdb",
    Ret = gen_server:call(Server,{closeconnection, CloseStr}, Timeout),
    ?DBG("sqlDisConnect gen_server call returns: ~p",[Ret]),
    case Ret of
	% Succeeded disconnecting  
	{ok, Mes,RetCode} ->
	    RetCode;
	% Did not succeded disconnecting (may already be disconnected)
	{error, ErrMsg, ErrCode} ->
	    {error, ErrMsg, ErrCode};
	Other ->
	    Other
    end.


%% sqlEndTran/[2, 3]
%%
%% Requests a commit or rollback operation for all active operations on all 
%% statement handles associated with a connection. It can only request a 
%% commit or rollback operation for a connection.
%%
%% sqlEndTran(Server, ComplType) ->
%% sqlEndTran(Server, ComplType, Timeout) ->
%%   Result | {error, ErrMsg, ErrCode}
%%
%% Server -> pid() |                    ; The pid of the server,
%%           Name |                       a registered name,
%%           {global, Name} |             a globally registered name, or
%%           {Name, Node}                 a registered name on a remote node.
%% ComplType -> ?SQL_COMMIT |           ; Commit operation.
%%              ?SQL_ROLLBACK             Rollback operation.
%% Timeout -> integer() |               ; timeout (ms) for genserver.
%%            infinity
%% Result -> ?SQL_SUCCESS |             
%%           ?SQL_SUCCESS_WITH_INFO
%% ErrMsg -> string()                   ; Error message.
%% ErrCode -> ?SQL_INVALID_HANDLE |
%%            ?SQL_ERROR

sqlEndTran(Server, ComplType) ->
    sqlEndTran(Server, ComplType, ?DefaultTimeout).

sqlEndTran(Server, ComplType, Timeout) ->
    AttrStr = integer_to_list(ComplType) ++ ";",
    CmdStr = "endtra" ++ AttrStr,

    Ret = gen_server:call(Server,{endTran, CmdStr},Timeout),  
    case Ret of
	% Succeeded in making ODBC call (which may have returned an error)
	{error, ErrMsg, ErrCode} ->
	    {error, ErrMsg, ErrCode};
	{ok, ReturnMsg, ReturnCode} ->
	    ReturnCode;
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	Other ->
	    Other
    end.
%% sqlExecDirect/[2, 3]
%% Executes a preparable statement.
%%
%% sqlExecDirect(Server, Stmt) ->
%% sqlExecDirect(Server, Stmt, Timeout) ->
%%   Result | {error, ErrMsg, ErrCode}
%%
%% Server -> pid() |                  ; The pid of the server,
%%           Name |                     a registered name,
%%           {global, Name} |           a globally registered name, or
%%           {Name, Node}               a registered name on a remote node.
%% Stmt -> string()                   ; An SQL statement.
%% Timeout -> integer() |             ; timeout (ms) for genserver
%%            infinity
%% Result -> ?SQL_SUCCESS |
%%           ?SQL_SUCCESS_WITH_INFO |
%%           ?SQL_NEED_DATA |
%%           ?SQL_NO_DATA |
%% ErrMsg -> string()                 ; The error message.
%% ErrCode -> ?SQL_INVALID_HANDLE |
%%            ?SQL_ERROR

sqlExecDirect(Server, Stmt) ->
    sqlExecDirect(Server, Stmt, ?DefaultTimeout).

sqlExecDirect(Server, Stmt, Timeout) ->
    chk_str(Stmt, sqlExecuteDirect, 2),
    SqlStmt = "exedir" ++ Stmt,
    Ret = gen_server:call(Server, {execdir,SqlStmt}, Timeout),
    ?DBG("sqlExecDirect gen_server call returns: ~p",[Ret]),
    case Ret of
	% Succeeded in making ODBC call (which may have returned an error)
	{error, ErrMsg, Errcode} ->
	    {error,ErrMsg,Errcode};
	{ok, Result, ReturnCode} ->
	    ReturnCode;
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	Other ->
	    Other
    end.

%% sqlFetch/[1, 2]
%% Fetches a row of data from a result set. The driver returns data for 
%% all columns that were bound to a referens with sqlBindCol/[3, 4].
%%
%% sqlFetch(Server) ->
%% sqlFetch(Server, Timeout) ->
%%   Result | {error, ErrMsg, ErrCode}
%%
%% Server -> pid() |                  ; The pid of the server,
%%           Name |                     a registered name,
%%           {global, Name} |           a globally registered name, or
%%           {Name, Node}               a registered name on a remote node.
%% Timeout -> integer() |             ; timeout (ms) for genserver
%%            infinity
%% Result -> ?SQL_SUCCESS |  
%%           ?SQL_SUCCESS_WITH_INFO |
%%           ?SQL_NO_DATA |
%% ErrMsg -> string()                 ; The error message.
%% ErrCode -> ?SQL_INVALID_HANDLE |
%%            ?SQL_ERROR

sqlFetch(Server) ->
    sqlFetch(Server, ?DefaultTimeout).

sqlFetch(Server, Timeout) ->
    CmdStr = "fetchd",
    Ret = gen_server:call(Server,{fetch, CmdStr},Timeout),
    ?DBG("sqlFetct gen_server call returns: ~p",[Ret]),
    case Ret of
	% Succeeded in making ODBC call (which may have returned an error)
	{error, ErrMsg, ErrCode} ->
	    {error, ErrMsg, ErrCode};
	{ok, ReturnCode} ->
	    ReturnCode;
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	Other ->
	    Other
    end.

%% sqlNumResultCols/[1, 2]
%% Returns the number of columns in a result set.
%%
%% sqlNumResultCols(Server) ->
%% sqlNumResultCols(Server, Timeout) ->
%%   {Result, ColCount} | {error, ErrMsg, ErrCode}
%%
%% Server -> pid() |                  ; The pid of the server,
%%           Name |                     a registered name,
%%           {global, Name} |           a globally registered name, or
%%           {Name, Node}               a registered name on a remote node.
%% Timeout -> integer() |             ; timeout (ms) for genserver
%%            infinity
%% Result -> ?SQL_SUCCESS |  
%%           ?SQL_SUCCESS_WITH_INFO
%% ColCount -> integer()              ; number of columns in the result set.
%% ErrMsg -> string()                 ; The error message.
%% ErrCode -> ?SQL_INVALID_HANDLE |
%%            ?SQL_ERROR

sqlNumResultCols(Server) ->
    sqlNumResultCols(Server, ?DefaultTimeout).

sqlNumResultCols(Server, Timeout) ->
    CmdStr = "rescol",
    Ret = gen_server:call(Server,{numResultCols, CmdStr},Timeout),
    ?DBG("sqlNumResultCols gen_server call returns: ~p",[Ret]),
    case Ret of
	% Succeeded in making ODBC call (which may have returned an error)
	{error, ErrMsg, ErrCode} ->
	    {error, ErrMsg, ErrCode};
	{ok, NumberColumn, ReturnCode} ->
	    {ReturnCode, NumberColumn};
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	Other ->
	    Other
    end.

%% sqlRowCount/[1, 2]
%% Returns the number of rows affected by an UPDATE, INSERT, or DELETE 
%% statement.
%%
%% sqlRowCount(Server) ->
%% sqlRowCount(Server, Timeout) ->
%%   {Result, RowCount} | {error, ErrMsg, ErrCode}
%%
%% Server -> pid() |                   ; The pid of the server,
%%           Name |                      a registered name,
%%           {global, Name} |            a globally registered name, or
%%           {Name, Node}                a registered name on a remote node.
%% Timeout -> integer() |              ; timeout (ms) for genserver.
%%            infinity
%% Result -> ?SQL_SUCCESS |
%%           ?SQL_SUCCESS_WITH_INFO
%% RowCount -> integer()               ; The number of affected rows (or -1
%%                                       if not available).
%% ColCount -> integer()              ; number of columns in the result set.
%% ErrMsg -> string()                 ; The error message.
%% ErrCode -> ?SQL_INVALID_HANDLE |
%%            ?SQL_ERROR

sqlRowCount(Server) ->
    sqlRowCount(Server, ?DefaultTimeout).

sqlRowCount(Server, Timeout) ->
    CmdStr = "rowcon",
    Ret = gen_server:call(Server,{rowCount, CmdStr},Timeout),
    ?DBG("sqlRowCount gen_server call returns: ~p",[Ret]),
    case Ret of
	% Succeeded in making ODBC call (which may have returned an error)
	{error, ErrMsg, ErrCode} ->
	    {error, ErrMsg, ErrCode};
	{ok, NumberColumn, ReturnCode} ->
	    {ReturnCode, NumberColumn};
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	Other ->
	    Other
    end.
    

%% sqlSetConnectAttr/[3, 4]
%%
%% Sets attributes that govern aspects of connections.
%% Driver-specific
%% attributes are not supported through macros, but can be set, if  they
%% are strings or signed/unsigned long integers.
%%
%% sqlSetConnectAttr(Server,Attr, Value) ->
%% sqlSetConnectAttr(Server, Attr, Value, Timeout) ->
%%   Result | {error, ErrMsg, ErrCode}
%%
%% Server -> pid() |                  ; The pid of the server,
%%           Name |                     a registered name,
%%           {global, Name} |           a globally registered name, or
%%           {Name, Node}               a registered name on a remote node.
%% Attr -> integer()                  ; Attribute number 
%% Value -> string() |                ; The new attribute value.
%%          integer()
%% Timeout -> integer() |             ; timeout (ms) for genserver.
%%            infinity
%% Result -> ?SQL_SUCCESS |
%%           ?SQL_SUCCESS_WITH_INFO 
%% ErrMsg -> string()                 ; The error message.
%% ErrCode -> ?SQL_INVALID_HANDLE |
%%            ?SQL_ERROR

sqlSetConnectAttr(Server, Attr, Value) ->
    sqlSetConnectAttr(Server, Attr, Value, ?DefaultTimeout).

sqlSetConnectAttr(Server, Attr, Value, Timeout) ->
    AttrStr = integer_to_list(Attr) ++ ";",
    ValueStr = if 
		   number(Value) ->
		       "0;" ++ integer_to_list(Value);
		   list(Value) ->
		       "1;" ++ Value;
		   true ->
		       exit({sqlSetConnectAttr, wrong_Value_type})
	       end,
    CmdStr = "setatr" ++ AttrStr ++ ValueStr ++ ";",

    Ret = gen_server:call(Server,{setConnectAttr, CmdStr},Timeout),  
    case Ret of
	% Succeeded in making ODBC call (which may have returned an error)
	{error, ErrMsg, ErrCode} ->
	    {error, ErrMsg, ErrCode};
	{ok, Result, ReturnCode} ->
	    ReturnCode;
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	Other ->
	    Other
    end.


%% columnRef/[0]
%% Generate a referens for a column. 
%%
%% columnRef() ->
%%   {ok, Ref}
%%
%% Ref -> term()             ; A reference.
%%

columnRef() ->
    Ref = erlang:make_ref(),
    {ok, Ref}.


%% readData/[2, 3]
%% Returns data from the sqlFetch/[1,2] call.
%%
%% readData(Server, Ref) ->
%% readData(Server, Ref, Timeout) ->
%%   {ok, Value}
%%
%% Server -> pid() |                    ; The pid of the server,
%%           Name |                       a registered name,
%%           {global, Name} |             a globally registered name, or
%%           {Name, Node}                 a registered name on a remote node.
%% Ref -> term()                        ; A reference to a column.
%% Timeout -> integer() |               ; timeout (ms) for genserver.
%%            infinity
%% Value -> string() |                  ; Returned Data 
%%

readData(Server, Ref) ->
    readData(Server, Ref, ?DefaultTimeout).

readData(Server, Ref, Timeout) ->
    CmdStr = "readbu",
    Ret = gen_server:call(Server, {readbuffer, CmdStr, Ref},Timeout),
    ?DBG("readData gen_server call returns: ~p",[Ret]),
    case Ret of
	% Succeeded in get data
	{ok, Data} ->
	    {ok, Data};
	% gen_server specific error
	{error, Reason} ->
	    exit(Reason);
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	Other ->
	    Other
    end.

%%%----------------------------------------------------------------------
%%% Utility API
%%%
%%%
%%%----------------------------------------------------------------------


%% erl_connect/[2, 3, 4, 5]
%% Opens a connection to a data source. There can be only one open data 
%% source connection per server. erl_connect/[2, 3] is used when the 
%% information that can be supplied through erl_connect/[4, 5] does not 
%% suffice.
%%
%% erl_connect(Server, ConnectStr) ->
%% erl_connect(Server, ConnectStr, Timeout) ->
%% erl_connect(Server, DSN, UID, PWD) ->
%% erl_connect(Server, DSN, UID, PWD, Timeout) ->
%%   ok |
%%   {error, ErrMsg, ErrCode}
%%
%% Server -> pid() |                      ; The pid of the server,
%%           Name |                         a registered name,
%%           {global, Name} |               a globally registered name, or
%%           {Name, Node}                   a registered name on a remote node.
%% ConnectStr -> string()                 ; 
%% Timeout -> integer() |                 ; timeotu (ms) for genserver.
%%            infinity
%% DSN -> string()                        ; Name of the database.
%% UID -> string()                        ; User ID.
%% PWD -> string()                        ; Password.
%% ErrMsg -> string()                     ; Error Message.
%% ErrCode -> ?SQL_ERROR
%%

erl_connect(Server, ConnectStr) ->
    erl_connect(Server, ConnectStr, ?DefaultTimeout).

erl_connect(Server, ConnectStr, Timeout) ->
    chk_str(ConnectStr, erl_connect, 2),
    NewConnectStr = "opendb" ++ ConnectStr,
    Ret = gen_server:call(Server,{openconnection, NewConnectStr, self()}, 
			  Timeout),
    ?DBG("erl_connect gen_server call returns: ~p",[Ret]),
    case Ret of
	% Server going down
	{stopped, Reason} ->
	    exit({stopped, Reason});
	% Succeeded connected 
	{ok, Msg, RetCode} ->
	    ok;
	% Did not succed connected
	{error, ErrMsg, ErrCode} ->
	    {error,ErrMsg, ErrCode};
	Other ->
	    Other
    end.

erl_connect(Server, DSN, UID, PWD) ->
    erl_connect(Server, DSN, UID, PWD, ?DefaultTimeout).

erl_connect(Server, DSN, UID, PWD, Timeout) ->
    chk_str(DSN, erl_connect, 2),
    chk_str(UID, erl_connect, 3),
    chk_str(PWD, erl_connect, 4),
    N = string:len(PWD),
    ConnectStr = case N of 
		     0 -> "DSN=" ++ DSN ++ ";UID=" ++ UID;
		     NotNull ->
			 "DSN=" ++ DSN ++ ";UID=" ++ UID ++ ";PWD="++ PWD
		 end,
    erl_connect(Server, ConnectStr, Timeout).


%% erl_executeStmt/[2, 3]
%% Executes a single SQL statement. All changes to the data source are
%% automatically committed if successful.
%%
%% erl_executeStmt(Server, Stmt) ->
%% erl_executeStmt(Server, Stmt, Timeout) ->
%%   {updated, NRows} |
%%   {selected, [ColName], [Row]} |
%%   {error, ErrMsg}
%%
%% Server -> pid() |                     ; The pid of the server,
%%           Name |                        a registered name,
%%           {global, Name} |              a globally registered name, or
%%           {Name, Node}                  a registered name on a remote node.
%% Stmt -> string()                      ; SQL statement to execute.
%% Timeout -> integer() |                ; timeout (ms) for genserver.
%%            infinity
%% NRows -> integer()                    ; The number of updated rows.
%% ColName -> string()                   ; The name of the columns in the table.
%% Row -> [Value]                        ; One row of the resulting table.
%%   Value -> string                     ; One value in a row.
%% ErrMsg -> string()                    ; Error message.

erl_executeStmt(Server, Stmt) ->
    erl_executeStmt(Server, Stmt, ?DefaultTimeout).

erl_executeStmt(Server, Stmt, Timeout) ->
    chk_str(Stmt, erl_executeStmt, 2),
    SqlStmt = "execdb" ++ Stmt,
    case gen_server:call(Server,{execute, SqlStmt}, Timeout)   of
	ok -> wait_result([],[]);
	    
	% Succeeded in making ODBC call (which may have returned an error)
	Other ->
	    Other
    end.
wait_result(ColumnNameList,RowList) ->
    receive
	{column_name,ColumnName} ->
	   ?DBG("erl_executeStmt -> ColumnName: ~p~n",[ColumnName]),
	   wait_result(ColumnName,RowList);
	{row,Data} ->
	    ?DBG("erl_executeStmt -> {row, Data}: ~p",[Data]),
	    wait_result(ColumnNameList,[Data|RowList]);
	{selected, lastrow} ->
	    {selected,ColumnNameList ,RowList};
	{updated, NupdatRow} ->
	    {updated, NupdatRow};
	{error, ErrMsg} ->
	    {error,ErrMsg};
	Other ->
	    ?DBG("erl_executeStmt -> Other: ~p~n",[Other]),
	    Other
    end.


%% erl_disconnect/[2, 3]
%% Closes the connection to a data source.
%%
%% erl_disconnect(Server, RefConnHandle) ->
%% erl_disconnect(Server, RefConnHandle, Timeout) ->
%%   ok |
%%   {error, ErrMsg, ErrCode}
%%
%% Server -> pid() |                     ; The pid of the server,
%%           Name |                        a registered name,
%%           {global, Name} |              a globally registered name, or
%%           {Name, Node}                  a registered name on a remote node.
%%
%% Timeout -> integer() |                ; timeout (ms) for genserver.
%%            infinity
%% ErrMsg -> string()                    ; Error messages
%% ErrCode -> integer()                  ; SQL errorcode
%%

erl_disconnect(Server, RefConnHandle) ->
    erl_disconnect(Server, RefConnHandle, ?DefaultTimeout).

erl_disconnect(Server, RefConnHandle, Timeout) ->
    CloseStr = "closdb",
    Ret = gen_server:call(Server,{closeconnection, CloseStr}, Timeout),
    ?DBG("erl_disconnect gen_server call returns: ~p",[Ret]),
    case Ret of
	% Succeeded disconnecting  
	{ok, Mes,RetCode} ->
	    ok;
	% Did not succeded disconnecting (may already be disconnected)
	{error, ErrMsg, ErrCode} ->
	    {error, ErrMsg, ErrCode};

	Other ->
	    {error, Other}
    end.

%%---------------------------------------------------------------------------
%%
%% Depricated functions
%%---------------------------------------------------------------------------

%% This function is deprecated and does nothing.
%% It is included only for compatibility reason.
%%--------------------------------------------------------------------------- 
sql_alloc_handle(Server, HandleType, RefInputHandle) ->
    sql_alloc_handle(Server, HandleType, RefInputHandle, ?DefaultTimeout).

sql_alloc_handle(Server, HandleType, RefInputHandle, Timeout) ->
    {0,void}. 

%% This function is deprecated and replaced by the function 
%% sqlBindColumn/[3,4].
%%---------------------------------------------------------------------------
sql_bind_col(Server, RefStmtHandle, ColNum, RefBuf) ->
    sql_bind_col(Server, RefStmtHandle, ColNum, RefBuf, ?DefaultTimeout).

sql_bind_col(Server, RefStmtHandle, ColNum, RefBuf, Timeout) ->
    Ret = sqlBindColumn(Server, ColNum, RefBuf, Timeout),
    case Ret of
	{error, ErrMsg, ErrCode} ->
	    ErrCode;
	Other ->
	    Other
    end.

%% This function is deprecated and replaced by the function
%% sqlCloseCursor/[1,2].
%%---------------------------------------------------------------------------
sql_close_cursor(Server, RefStmtHandle) ->
    sql_close_cursor(Server, RefStmtHandle, ?DefaultTimeout).

sql_close_cursor(Server, RefStmtHandle, Timeout) ->
    Ret = sqlCloseCursor(Server, Timeout),
    case Ret of
	{error, ErrMsg, ErrCode} ->
	    ErrCode;
	Other ->
	    Other
    end.

%% This function is deprecated and replaced by the function
%% sqlDescribeCol/[2,3].
%%---------------------------------------------------------------------------
sql_connect(Server, RefConnHandle, DSN, UID, Auth) ->
    sql_connect(Server, RefConnHandle, DSN, UID, Auth, ?DefaultTimeout).

sql_connect(Server, RefConnHandle, DSN, UID, Auth, Timeout) ->
    Ret = sqlConnect(Server, DSN, UID, Auth, Timeout),
    case Ret of
	{error, ErrMsg, ErrCode} ->
	    ErrCode;
	Other ->
	    Other
    end.

%% This function is deprecated and repleced by the function
%% sqlDescribeCol/[2,3].
%%---------------------------------------------------------------------------
sql_describe_col(Server, RefStmtHandle, ColNum, BufLenColName) ->
    sql_describe_col(Server, RefStmtHandle, ColNum, BufLenColName, ?DefaultTimeout).

sql_describe_col(Server, RefStmtHandle, ColNum, BufLenColName, Timeout) ->
    Ret = sqlDescribeCol(Server, ColNum, Timeout),
    case Ret of
	{error, ErrMsg, ErrCode} ->
	    ErrCode;
	{ReturnCode, ColName, CNullable} ->
	    {ReturnCode, {ColName, -1}, -1, -1, -1, CNullable};
	Other ->
	    Other
    end.

%% This function is deprecated and replaced by the function
%% sqlDisConnect/[1,2].
%%---------------------------------------------------------------------------
sql_disconnect(Server, RefConnHandle) ->
    sql_disconnect(Server, RefConnHandle, ?DefaultTimeout).

sql_disconnect(Server, RefConnHandle, Timeout) ->
    Ret = sqlDisConnect(Server, Timeout),
    case Ret of
	{error, ErrMsg, ErrCode} ->
	    ErrCode;
	Other ->
	    Other
    end.	

%% This function is deprecated and does nothing.
%% It is included only for compatibility reason.
%%--------------------------------------------------------------------------
sql_driver_connect(Server, RefConnHandle, InConnStr,
		   BufLenOutConnStr, DrvCompletion) ->
    sql_driver_connect(Server, RefConnHandle, InConnStr,
		       BufLenOutConnStr, DrvCompletion, ?DefaultTimeout).

sql_driver_connect(Server, RefConnHandle, InConnStr,
		   BufLenOutConnStr, DrvCompletion, Timeout) ->
    {0, void}.


%% This function is deprecated and replaced by the function 
%% sqlEndTran/[2,3]
%%---------------------------------------------------------------------------
sql_end_tran(Server, HandleType, RefHandle, ComplType) ->
    sql_end_tran(Server, HandleType, RefHandle, ComplType, ?DefaultTimeout).

sql_end_tran(Server, HandleType, RefHandle, ComplType, Timeout) ->
    Ret = sqlEndTran(Server, ComplType, Timeout),
    case Ret of
	{error, ErrMsg, ErrCode} ->
	    ErrCode;
	Other ->
	    Other
    end.


%% This function is deprecated and replaced by the function
%% sqlExecDirect/[2,3].
%%---------------------------------------------------------------------------
sql_exec_direct(Server, RefStmtHandle, Stmt) ->
    sql_exec_direct(Server, RefStmtHandle, Stmt, ?DefaultTimeout).


sql_exec_direct(Server, RefStmtHandle, Stmt, Timeout) ->
    Ret = sqlExecDirect(Server, Stmt, Timeout),
    case Ret of
	{error, ErrMsg, ErrCode} ->
	    ErrCode;
	Other ->
	    Other
    end.

%% This function is deprecated and replaced by the function
%% sqlFetch/[1,2].
%%---------------------------------------------------------------------------
sql_fetch(Server, RefStmtHandle) ->
    sql_fetch(Server, RefStmtHandle, ?DefaultTimeout).

sql_fetch(Server, RefStmtHandle, Timeout) ->
    Ret = sqlFetch(Server, Timeout),
    case Ret of
	{error, ErrMsg, ErrCode} ->
	    ErrCode;
	Other ->
	    Other
    end.


%% This function is deprecated and does nothing.
%% It is included only for compatibility reason.
%%---------------------------------------------------------------------------
sql_free_handle(Server, HandleType, RefHandle) ->
    sql_free_handle(Server, HandleType, RefHandle, ?DefaultTimeout).

sql_free_handle(Server, HandleType, RefHandle, Timeout) ->
    0.

%% This function is deprecated and does nothing
%% It is included only for compatibility reason.
%%---------------------------------------------------------------------------
sql_get_connect_attr(Server, RefConnHandle, Attr, BufType) ->
    sql_get_connect_attr(Server, RefConnHandle, Attr, BufType, 
			 ?DefaultTimeout).

sql_get_connect_attr(Server, RefConnHandle, Attr, BufType, Timeout) ->
    {0, -1}.

%% This function is deprecated and does nothing.
%% It is included only for compatibility reason.
%%---------------------------------------------------------------------------
sql_get_diag_rec(Server, HandleType, RefHandle, RecNum, BufLenErrMsg) ->
    sql_get_diag_rec(Server, HandleType, RefHandle, RecNum, BufLenErrMsg,
		     ?DefaultTimeout).

sql_get_diag_rec(Server, HandleType, RefHandle, RecNum, BufLenErrMsg, 
		 Timeout) ->
   0.

%% This function is deprecated and replaced by the function
%% sqlNumResultCols/[1,2].
%%---------------------------------------------------------------------------
sql_num_result_cols(Server, RefStmtHandle) ->
    sql_num_result_cols(Server, RefStmtHandle, ?DefaultTimeout).

sql_num_result_cols(Server, RefStmtHandle, Timeout) ->
    Ret = sqlNumResultCols(Server, Timeout),
    case Ret of
	{error, ErrMsg, ErrCode} ->
	    ErrCode;
	 Other ->
	    Other
    end.    

%% This function is deprecated and replaced by the function
%% sql_rowCount/[1,2].
%%---------------------------------------------------------------------------
sql_row_count(Server, RefStmtHandle) ->
    sql_row_count(Server, RefStmtHandle, ?DefaultTimeout).

sql_row_count(Server, RefStmtHandle, Timeout) ->
    Ret = sqlRowCount(Server, Timeout),
    case Ret of
	{error, ErrMsg, ErrCode} ->
	    ErrCode;
	 Other ->
	    Other
    end.

%% This function is deprecated and replaced by the function.
%% sqlSetConnectAttr/[4, 5].
%%---------------------------------------------------------------------------
sql_set_connect_attr(Server, RefConnHandle, Attr, Value, BufType) ->
    sql_set_connect_attr(Server, RefConnHandle, Attr, Value, BufType,
			 ?DefaultTimeout).

sql_set_connect_attr(Server, RefConnHandle, Attr, Value, BufType, Timeout) ->
    Ret = sqlSetConnectAttr(Server,Attr,Value,Timeout),
    case Ret of
	{error, ErrMsg, ErrCode} ->
	    ErrCode;
	Other ->
	    Other
    end.

%% This function is deprecated and does nothing.
%% It is included only for compatibility reason.
%%---------------------------------------------------------------------------
sql_set_env_attr(Server, RefEnvHandle, Attr, Value, BufType) ->
    sql_set_env_attr(Server, RefEnvHandle, Attr, Value, BufType, 
		     ?DefaultTimeout).

sql_set_env_attr(Server, RefEnvHandle, Attr, Value, BufType, Timeout) ->
    0.

%% This function is deprecated and replaced by the function
%% columnRef/0. 
%%---------------------------------------------------------------------------
alloc_buffer(Server, BufCType, Size) ->
    alloc_buffer(Server, BufCType, Size, ?DefaultTimeout).

alloc_buffer(Server, BufCType, Size, Timeout) ->
    columnRef().

%% This function is deprecated and does nothing.
%% It is included only for compatibility reason.
%%---------------------------------------------------------------------------
dealloc_buffer(Server, RefBuf) ->
    dealloc_buffer(Server, RefBuf, ?DefaultTimeout).

dealloc_buffer(Server, RefBuf, Timeout) ->
    ok.

%% This function is deprecated and replaced by the function 
%% readData/[2,3].
%%---------------------------------------------------------------------------
read_buffer(Server, RefBuf) ->
    read_buffer(Server, RefBuf, ?DefaultTimeout).

read_buffer(Server, RefBuf, Timeout) ->
    Ret = readData(Server, RefBuf, Timeout),
    case Ret of
	{ok, Value} ->
	    {ok, {Value, -1}}; 
	Other ->
	    Other
    end.

%% This function is deprecated and does nothing.
%% It is included only for compatibility reason.
%%---------------------------------------------------------------------------
init_env(Server) ->
    init_env(Server, ?DefaultTimeout).

init_env(Server, Timeout) ->
    {ok, void}.


%% This function is deprecated and replaced by the function 
%% erl_connect/[2,3,4,5].
%%---------------------------------------------------------------------------
connect(Server, RefEnvHandle, ConnectStr) ->
    connect(Server, RefEnvHandle, ConnectStr, ?DefaultTimeout).

connect(Server, RefEnvHandle, ConnectStr, Timeout) ->
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
    connect(Server, RefEnvHandle, DSN, UID, PWD, ?DefaultTimeout).

connect(Server, RefEnvHandle, DSN, UID, PWD, Timeout) ->
    Ret = erl_connect(Server, DSN, UID, PWD, Timeout),
    case Ret of
	ok ->
	    {ok, void};
	{error,ErrMsg, ErrCode} ->
	    {error, {connect, [{void, {ErrCode, ErrMsg, -1}}]}};
	Other ->
	    Other
    end.

%% This function is deprecated and replaced by the function
%% erl_executeStmt/[2,3].
%%---------------------------------------------------------------------------
execute_stmt(Server, RefConnHandle, Stmt) ->
    execute_stmt(Server, RefConnHandle, Stmt, ?DefaultTimeout).

execute_stmt(Server, RefConnHandle, Stmt, Timeout) ->
    Ret = erl_executeStmt(Server, Stmt, Timeout),
    case Ret of
	{error,ErrMsg, ErrMsg} ->
	    {error,{execute_stmt, [{void, {-1, ErrMsg, -1}}]}};
	{selected, ColumnNameList, RowData} -> 
	    NewRowData = bar(RowData),
	    {selected,ColumnNameList, NewRowData};
	Other ->
	    Other
    end.

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

    
%% This function is deprecated and replaced by the function
%% erl_disconnect/[1,2].
%%---------------------------------------------------------------------------
disconnect(Server, RefConnHandle) ->
    disconnect(Server, RefConnHandle, ?DefaultTimeout).

disconnect(Server, RefConnHandle, Timeout) ->
    Ret = erl_disconnect(Server, Timeout),
    case Ret of
	ok ->
	    ok;
	{error, ErrMsg, ErrCode} ->
	    {error, {disconnect, [{void, {ErrCode, ErrMsg, -1}}]}};
	Other ->
	    Other
    end.


%% This function is deprecated and does nothing.
%% It is included only for compatibility reason.
%%-------------------------------------------------------------------
terminate_env(Server, RefEnvHandle) ->
    terminate_env(Server, RefEnvHandle, ?DefaultTimeout).

terminate_env(Server, RefEnvHandle, Timeout) ->
    ok.

%% This function is deprecated and does nothing.
%% It is included only for compatibility reason.
%%-------------------------------------------------------------------
display_size(SqlType, ColumnSize) when 
  integer(SqlType), integer(ColumnSize) ->
    -1.


%%%----------------------------------------------------------------------
%%% Debug fcns
%%%----------------------------------------------------------------------

%% Returns the internal state in a tagged tuple list.
%%
get_state(Server) ->
    State = gen_server:call(Server, get_state),
    [{port, State#state.port},
     {connected, State#state.connected},
     {fetchdata, State#state.fetchdata},
     {clientPid, State#state.clientPid},
     {columnvector, State#state.columnvector}].


%%%======================================================================
%%% Callback functions from gen_server
%%%======================================================================

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          {stop, Reason}
%%----------------------------------------------------------------------

%%
init(Args) ->
    process_flag(trap_exit, true),
    % Start the C node
    case os:find_executable(?ServerProg, ?ServerDir) of
	FileName when list(FileName)->
	    Port = open_port({spawn,FileName},[{packet,4}, binary]),
	    State = #state{port=Port},
	    {ok, State};
	false ->
	    {stop, {no_odbcserver, "Can't find executable."}}
    end.
		    

%%----------------------------------------------------------------------
%% Func: handle_call/3
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

%%---------------------------------------------------------------------
%% Stop Call Backs
%%---------------------------------------------------------------------

% stop
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

%%---------------------------------------------------------------------
%% ODBC Call Backs
%%---------------------------------------------------------------------

handle_call({openconnection,ConnectString,ClientPid}, _, State) ->
    Port = State#state.port,
    Reply = send(Port, ConnectString),
    NewState = State#state{connected = true, clientPid = ClientPid},
    {reply, Reply, NewState};

handle_call({execute, SqlStmt}, _, State) ->
    Port = State#state.port,
    Connected = State#state.connected,
    Reply = case Connected of
		false ->
		    {error, not_connected};
		true ->
		    Port ! {self(), {command, term_to_binary(SqlStmt)}},
		    ok
	    end,
    {reply, Reply, State};
handle_call({execdir, CmdStr}, _, State) ->
    Port = State#state.port,
    Connected = State#state.connected,
    Reply =
	case Connected of
	    false ->
		{error, not_connected};
	    true ->
		send(Port, CmdStr)
	end,
    {reply, Reply, State};

handle_call({closeconnection,CloseStr}, _,State) ->
    Port = State#state.port,
    NewState = State#state{connected = false},
    Reply = send(Port, CloseStr),
    {reply,Reply, NewState};

handle_call({bindcolumn, CmdStr, ColNum, MemRef}, _,State) ->
    Port = State#state.port,
    Connected = State#state.connected,
    ColumnVec = State#state.columnvector,
    NewColumnVec = insert({MemRef,ColNum}, ColumnVec),
    ?DBG("handle_call: bindcolumn insert return ~p",[NewColumnVec]),
    NewState = State#state{columnvector = NewColumnVec},
    NCmdStr = CmdStr ++ integer_to_list(ColNum),
    Reply =
	case Connected of
	    false ->
		{error, not_connected};
	    true ->
		send(Port, NCmdStr)
	end,
    {reply,Reply, NewState};

handle_call({closecursor, CmdStr}, _, State) ->
    Port = State#state.port,
    Connected = State#state.connected,
    ColumnVect = State#state.columnvector,
    NewColumnVect = delete(ColumnVect),
    NewState = State#state{columnvector = NewColumnVect, fetchdata = false},
    Reply = case Connected of
		false ->
		    {error, not_connected};
		true ->
		    send(Port, CmdStr)
	    end,
    {reply,Reply, NewState};

handle_call({describeColumn, CmdStr}, _, State) ->
    Port = State#state.port,
    Connected = State#state.connected,
    Reply =
	case Connected of
	    false ->
		{error, not_connected};
	    true ->
		send(Port, CmdStr)
	end,
    {reply,Reply, State};

handle_call({endTran, CmdStr}, _, State) ->
    Port = State#state.port,
    Connected = State#state.connected,
    Reply =
	case Connected of
	    false ->
		{error, not_connected};
	    true ->
		send(Port, CmdStr)
	end,
    {reply,Reply, State};

handle_call({fetch, CmdStr}, _, State) ->
    Port = State#state.port,
    Connected = State#state.connected,
    NewState =  State#state{fetchdata = true},
    Reply = case Connected of
		false ->
		    {error, not_connected};
		true ->
		    send(Port, CmdStr)
	    end,
    {reply,Reply, NewState};

handle_call({numResultCols, CmdStr}, _, State) ->
    Port = State#state.port,
    Connected = State#state.connected,
    Reply =
	case Connected of
	    false ->
		{error, not_connected};
	    true ->
		send(Port, CmdStr)
	end,
    {reply,Reply, State};

handle_call({rowCount, CmdStr}, _, State) ->
    Port = State#state.port,
    Connected = State#state.connected,
    Reply =
	case Connected of
	    false ->
		{error, not_connected};
	    true ->
		send(Port, CmdStr)
	end,
    {reply,Reply, State};

handle_call({setConnectAttr, CmdStr}, _, State) ->
    Port = State#state.port,
    Reply = send(Port, CmdStr),
    {reply,Reply, State};

handle_call({readbuffer, CmdStr, ColumnRef}, _, State) ->
    Port = State#state.port,
    ColumnVect = State#state.columnvector,
    CNumber = lockup(ColumnRef,ColumnVect),
    Fetchdata = State#state.fetchdata,
    Reply = case CNumber of
		not_found ->
		    {error, column_found};
		Other ->
		    case Fetchdata of
			false ->
			    {error, no_data_fetched};
			true ->
			    NCmdStr = CmdStr ++ integer_to_list(CNumber),
			    send(Port, NCmdStr)
		    end
	    end,
    {reply,Reply, State};

handle_call(stop, _, State) ->
    ?DBG("handle_call(stop)~n",[]),
    {stop,  normal, ok, State};

%%---------------------------------------------------------------------
%% Debug Call Back
%%---------------------------------------------------------------------

handle_call(get_state, _From, State) ->
    {reply, State, State};


%%-----------------------------------------------------------

% Unknown request
handle_call(Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.
%%----------------------------------------------------------------------


%%----------------------------------------------------------------------
%% Func: handle_cast/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: handle_info/2
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%%----------------------------------------------------------------------

handle_info({'EXIT', Pid, Reason}, State) when pid(Pid) ->
    ok =
	error_logger:error_report({"ODBC: Linked process:", 
				   {'EXIT', Pid, Reason}}),
    {stop, {stopped, {'EXIT', Pid, Reason}}, State};
% EXIT from port.
% Log exit and terminate.
%
handle_info({'EXIT', Port, Reason}, State) ->
    ok =
	error_logger:error_report({"ODBC: Port:", {'EXIT', Port, Reason}}),
    {stop, {stopped, {'EXIT', Port, Reason}}, State};

handle_info({Port, {data, BinData}}, State) ->
    ClientPid = State#state.clientPid,
    case (catch binary_to_term(BinData)) of
	{'EXIT',Reason} ->
	    ?DBG("handle_info(Port) decode error: ~p~n", [Reason]),
	    {error, {binary_to_term, Reason}};
	Data ->
	    ?DBG("handle_info(Port) Data: ~p~n",[Data]),
	    ClientPid ! Data,
	    Data
    end,
    {noreply, State};

% Other.
%
handle_info(_Info, State) ->
    {noreply, State}.

%%----------------------------------------------------------------------
%% Func: terminate/2
%% Purpose: Shutdown the server
%% Returns: any (ignored by gen_server)
%%----------------------------------------------------------------------
terminate(Reason, State) ->
    Port = State#state.port,
    Port ! {self(), close}.



%%%======================================================================
%%% Internal functions
%%%======================================================================


%%%---------------------------------------------------------------------
%%% Argument checking
%%%---------------------------------------------------------------------
%%% These fcns check input arguments to the interface
%%% fcns. They all return ok. If an argument is not
%%% valid, the following exception thrown:
%%% {badarg, F, ArgNo, Info}
%%%   F -> atom()             ; Function.
%%%   ArgNo -> integer()      ; The ordinal number of the argument in the
%%%                             interface fcn.
%%%   Info -> term()          ; Error info.
%%%
%%%---------------------------------------------------------------------


%%------------------------------------------------------------------
%% chk_str/3
%%
%% Checks that a string is valid.
%% NOTE: Does not check that the string contains ASCII
%%       characters only or that it doesn't contain a
%%       null-termination character!
%%
%% chk_str(Str, CallingFunc, ArgNo) -> ok
%%

chk_str(Str, _CallingFunc, _ArgNo) when list(Str) ->
    ok;
chk_str(Str, CallingFunc, ArgNo) ->
    exit({badarg,
	  CallingFunc,
	  ArgNo,
	  {"Not a string", Str}}).

%%------------------------------------------------------------------
%% chk_int/3
%%
%% Checks that arg is a integer.
%% chk_int(Int, CallingFunc, ArgNo) -> ok
%%

chk_int(Int, _CallingFunc, _ArgNo) when integer(Int) ->
    ok;
chk_int(Int, CallingFunc, ArgNo) ->
    exit({badarg,
	  CallingFunc,
	  ArgNo,
	  {"Not a Integer", Int}}).



%%%----------------------------------------------------------------
%%% Other Internal Fcns
%%%----------------------------------------------------------------


%%----------------------------------------------------------------
%% get_port_reply/1
%%
%% get_port_reply(Port) -> ok | {error, Reason}
%%
%%

%% Send a command to ODBC
send(Port, Data)->
    Port ! {self(), {command, term_to_binary(Data)}},
    get_port_reply(Port).
%% Wait of respond from ODBC
get_port_reply(Port)->
    receive
	{Port, {data, BinData}}  -> 
	    case (catch binary_to_term(BinData)) of
		{'EXIT',Reason} ->
		    {error, {binary_to_term, Reason}};
		Data ->
		    Data
	    end;
	{'EXIT', P, Reason} ->
	    {error, port_died};
	Other ->
	    ?DBG("get_prot_reply Other ~p ~n", [Other])
    end.

%% This is a simple table. The table contains tuples.
%% Each tuple contain a memoryreferens and a corrsponding
%% columnnumber.

insert({Ref, ColumnNr}, []) ->
    [{Ref, ColumnNr}];
insert({Ref, ColumnNr}, Xs) ->
    [{Ref, ColumnNr}| Xs].

lockup(Ref, []) ->
    not_found;
lockup(Ref, [{Ref, ColumnNr} | Xs]) ->
    ColumnNr;
lockup(Ref, [{ORef, ColumnNr} | Xs] ) ->
    lockup(Ref, Xs).

delete(Xs) ->
    [].



















